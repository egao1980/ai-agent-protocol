(in-package #:ai-agent-protocol/tests)

;;; Approval replies (opencode v2): approve once, approve always (records an
;;; allow rule for the rest of the run and saves it to the permission rule
;;; store), reject (optionally every pending request of the run).

(defun %edit-call (id path)
  (make-llm-tool-call-part :id id :name "edit"
                           :arguments (format nil "{\"path\":~s}" path)))

(defun %calls-response (&rest calls)
  (make-llm-response :parts calls :finish-reason :tool-use))

(defun %path-of (arguments)
  "The path of an edit call's JSON ARGUMENTS (enough for these tests)."
  (let* ((start (+ (search "\"path\":\"" arguments) 8))
         (end (position #\" arguments :start start)))
    (subseq arguments start end)))

(defun %ask-edit-agent (ran responses &key (n (list 0)) permissions settings store)
  "Agent whose model answers RESPONSES in turn; edit asks by default and
reports its path as the resource. RAN collects the edited paths."
  (let ((agent (make-ai-agent :name "coder"
                              :backend (make-mock-llm-backend
                                        :handler (%counting-handler n responses))
                              :permissions (or permissions '(("edit" "*" :ask)))
                              :permission-store store
                              :settings settings)))
    (register-agent-tool
     agent
     (make-function-tool :name "edit"
                         :permission-resources (lambda (args) (list (%path-of args)))
                         :handler (lambda (args)
                                    (push (%path-of args) (car ran))
                                    "edited")))
    agent))

(defun %invocation (run id)
  (find id (agent-run-invocations run) :key #'agent-invocation-id :test #'equal))

(deftest approve-always-allows-later-matching-calls
  (with-agent-loop
    (let* ((ran (list nil))
           (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                             (%calls-response (%edit-call "c2" "src/a.lisp"))
                                             (%calls-response (%edit-call "c3" "src/b.lisp"))
                                             (%text-response "done"))))
           (run (run-ai-agent agent "go")))
      (ok (eq :approval (agent-run-finish-reason run)))
      (approve-invocation run (first (agent-run-pending run)) :always t)
      (let ((rules (agent-run-approved-permissions run)))
        (ok (= 1 (length rules)))
        (ok (equal "edit" (permission-rule-action (first rules))))
        (ok (equal "src/a.lisp" (permission-rule-resource (first rules))))
        (ok (eq :allow (permission-rule-effect (first rules)))))
      (let ((run2 (resume-ai-agent run)))
        (ok (eq :approval (agent-run-finish-reason run2))
            "a call on another resource still asks")
        (ok (eq :done (agent-invocation-status (%invocation run2 "c2")))
            "the matching call ran without asking")
        (ok (equal '("c3") (mapcar #'agent-invocation-id (agent-run-pending run2))))
        (approve-invocation run2 (first (agent-run-pending run2)))
        (let ((run3 (resume-ai-agent run2)))
          (ok (eq :stop (agent-run-finish-reason run3)))
          (ok (equal '("src/b.lisp" "src/a.lisp" "src/a.lisp") (car ran))))))))

(deftest approve-always-approves-matching-pending-calls
  (with-agent-loop
    (let* ((ran (list nil))
           (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                                              (%edit-call "c2" "src/a.lisp")
                                                              (%edit-call "c3" "src/b.lisp"))
                                             (%text-response "done"))))
           (run (run-ai-agent agent "go")))
      (ok (= 3 (length (agent-run-pending run))))
      (approve-invocation run (%invocation run "c1") :always t)
      (ok (equal '("c3") (mapcar #'agent-invocation-id (agent-run-pending run)))
          "the other pending call on the same resource is approved too")
      (ok (eq :approved (agent-invocation-status (%invocation run "c2")))))))

(deftest approve-always-saves-to-the-rule-store
  (with-agent-loop
    (let* ((ran (list nil))
           (store (make-in-memory-permission-rule-store))
           (n (list 0))
           (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                             (%text-response "done"))
                                   :n n :store store))
           (run (run-ai-agent agent "go")))
      (ok (typep store 'permission-rule-store))
      (ok (null (list-permission-rules store)))
      (approve-invocation run (first (agent-run-pending run)) :always t)
      (let ((saved (list-permission-rules store)))
        (ok (= 1 (length saved)) "the store sees the saved rule")
        (ok (equal '("edit" "src/a.lisp" :allow)
                   (list (permission-rule-action (first saved))
                         (permission-rule-resource (first saved))
                         (permission-rule-effect (first saved))))))
      (ok (eq :stop (agent-run-finish-reason (resume-ai-agent run))))
      (testing "saving the same rule twice keeps one"
        (save-permission-rule store (make-permission-rule "edit" "src/a.lisp" :allow))
        (ok (= 1 (length (list-permission-rules store)))))
      (testing "a later run with the same store does not ask"
        (setf (car n) 0)
        (let ((run2 (run-ai-agent agent "again")))
          (ok (eq :stop (agent-run-finish-reason run2)))
          (ok (eq :done (agent-invocation-status (%invocation run2 "c1"))))))
      (testing "run settings :permission-store overrides the agent's"
        (setf (car n) 0)
        (let* ((other (make-in-memory-permission-rule-store))
               (run3 (run-ai-agent agent "third"
                                   :settings (make-agent-settings
                                              :extra (list :permission-store other)))))
          (ok (eq :approval (agent-run-finish-reason run3)))
          (approve-invocation run3 (first (agent-run-pending run3)) :always t)
          (ok (= 1 (length (list-permission-rules other)))))))))

(deftest saved-allow-never-overrides-a-deny
  (with-agent-loop
    (flet ((store-allowing-edit ()
             (make-in-memory-permission-rule-store
              :rules (list (make-permission-rule "edit" "*" :allow)))))
      (testing "policy deny wins over a saved allow"
        (let* ((ran (list nil))
               (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                                 (%text-response "done"))
                                       :store (store-allowing-edit)
                                       :settings (make-agent-settings
                                                  :extra (list :permission-policy
                                                               '(("edit" "*" :deny))))))
               (run (run-ai-agent agent "go")))
          (ok (eq :stop (agent-run-finish-reason run)))
          (ok (eq :denied (agent-invocation-status (%invocation run "c1"))))
          (ok (null (car ran)))))
      (testing "an agent-layer deny wins over a saved allow"
        (let* ((ran (list nil))
               (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "secret/k"))
                                                 (%text-response "done"))
                                       :permissions '(("edit" "*" :ask)
                                                      ("edit" "secret/*" :deny))
                                       :store (store-allowing-edit)))
               (run (run-ai-agent agent "go")))
          (ok (eq :denied (agent-invocation-status (%invocation run "c1"))))
          (ok (null (car ran)))))
      (testing "an always rule from this run does not loosen a policy ask"
        (let* ((ran (list nil))
               (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                                 (%calls-response (%edit-call "c2" "src/a.lisp"))
                                                 (%text-response "done"))
                                       :permissions '(("edit" "*" :allow))
                                       :settings (make-agent-settings
                                                  :extra (list :permission-policy
                                                               '(("edit" "*" :ask))))))
               (run (run-ai-agent agent "go")))
          (approve-invocation run (first (agent-run-pending run)) :always t)
          (let ((run2 (resume-ai-agent run)))
            (ok (eq :approval (agent-run-finish-reason run2)))
            (ok (equal '("c2") (mapcar #'agent-invocation-id (agent-run-pending run2))))))))))

(deftest deny-all-rejects-every-pending-invocation
  (with-agent-loop
    (flet ((three-edits (ran)
             (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                                         (%edit-call "c2" "src/b.lisp")
                                                         (%edit-call "c3" "src/c.lisp"))
                                        (%text-response "done")))))
      (testing "single reject keeps the others pending"
        (let* ((run (run-ai-agent (three-edits (list nil)) "go")))
          (deny-invocation run (%invocation run "c1") :feedback "No.")
          (ok (equal '("c2" "c3") (mapcar #'agent-invocation-id (agent-run-pending run))))))
      (testing ":all t rejects every pending invocation with the feedback"
        (let* ((ran (list nil))
               (run (run-ai-agent (three-edits ran) "go")))
          (ok (= 3 (length (agent-run-pending run))))
          (deny-invocation run (%invocation run "c2") :feedback "Stop editing; explain first." :all t)
          (ok (null (agent-run-pending run)))
          (dolist (id '("c1" "c2" "c3"))
            (let ((inv (%invocation run id)))
              (ok (eq :denied (agent-invocation-status inv)))
              (ok (agent-invocation-error-p inv))
              (ok (search "Stop editing; explain first." (agent-invocation-result inv)))))
          (let ((run2 (resume-ai-agent run)))
            (ok (eq :stop (agent-run-finish-reason run2)))
            (ok (null (car ran))))))
      (testing ":all t without feedback uses the plain reason"
        (let* ((run (run-ai-agent (three-edits (list nil)) "go")))
          (deny-invocation run (%invocation run "c1") :all t)
          (ok (null (agent-run-pending run)))
          (ok (every (lambda (inv) (equal "denied" (agent-invocation-result inv)))
                     (agent-run-invocations run))))))))

(deftest approval-restarts-always-and-deny-all
  (with-agent-loop
    (testing "INVOKE-DENY-ALL rejects the rest of the batch"
      (let* ((ran (list nil))
             (asked 0)
             (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                                                (%edit-call "c2" "src/b.lisp"))
                                               (%text-response "done"))))
             (run (handler-bind ((agent-approval-required
                                   (lambda (c)
                                     (incf asked)
                                     (invoke-deny-all c "Not now."))))
                    (run-ai-agent agent "go"))))
        (ok (= 1 asked))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (null (car ran)))
        (ok (every (lambda (inv) (search "Not now." (agent-invocation-result inv)))
                   (agent-run-invocations run)))))
    (testing "INVOKE-APPROVE-ALWAYS approves matching calls without asking again"
      (let* ((ran (list nil))
             (asked 0)
             (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                                                (%edit-call "c2" "src/a.lisp"))
                                               (%calls-response (%edit-call "c3" "src/a.lisp"))
                                               (%text-response "done"))))
             (run (handler-bind ((agent-approval-required
                                   (lambda (c)
                                     (incf asked)
                                     (invoke-approve-always c))))
                    (run-ai-agent agent "go"))))
        (ok (= 1 asked))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (= 3 (length (car ran))))))))

(deftest approve-always-replays-from-the-journal
  (with-agent-loop
    (let* ((n (list 0))
           (ran (list nil))
           (journal (task-protocol:make-in-memory-journal))
           (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                             (%calls-response (%edit-call "c2" "src/a.lisp"))
                                             (%text-response "done"))
                                   :n n))
           (dur (ai-agent-protocol/durability:make-agent-durability
                 :journal journal :task-id "always-run"))
           (run (run-ai-agent agent "go" :durability dur)))
      (ok (eq :approval (agent-run-finish-reason run)))
      (approve-invocation run (first (agent-run-pending run)) :always t)
      (let ((run2 (resume-ai-agent run)))
        (ok (eq :stop (agent-run-finish-reason run2)))
        (ok (= 3 (car n)))
        (ok (= 2 (length (car ran)))))
      (let* ((journal2 (task-protocol:copy-in-memory-journal journal))
             (dur2 (ai-agent-protocol/durability:make-agent-durability
                    :journal journal2 :task-id "always-run"))
             (run3 (run-ai-agent agent "go" :durability dur2)))
        (ok (eq :stop (agent-run-finish-reason run3))
            "replay restores the always rule, so c2 does not pause")
        (ok (= 3 (car n)) "no generate re-runs")
        (ok (= 2 (length (car ran))) "no tool re-runs")
        (ok (= 1 (length (agent-run-approved-permissions run3))))))))
