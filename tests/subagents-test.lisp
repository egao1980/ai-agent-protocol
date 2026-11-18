(in-package #:ai-agent-protocol/tests)

;;; Background subagents: the tool returns an id at once, the child's last
;;; reply reaches the parent's inbox as <subagent …>, follow-ups by id, one
;;; level only, the parent waits for live children, cancel cascades.

(defun %last-turn (turns)
  (car (last turns)))

(defun %reports (turns)
  "User turns that carry a <subagent> report."
  (remove-if-not (lambda (turn)
                   (and (eq :user (llm-turn-role turn))
                        (search "<subagent " (turn-text turn))))
                 turns))

(defun %call (id name arguments)
  (make-llm-response
   :parts (list (make-llm-tool-call-part :id id :name name :arguments arguments))
   :finish-reason :tool-use))

(defun %say (text)
  (make-llm-response :parts (list (make-llm-text-part :text text))
                     :finish-reason :stop))

(defun %slow-echo (delay)
  "Child backend: echoes the last user text after DELAY seconds (off-loop)."
  (make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b))
              (sleep delay)
              (%say (format nil "echo: ~a" (turn-text (%last-turn turns)))))))

(defun %parent-backend (plan)
  "PLAN is (lambda (turns reports)) → response; REPORTS counts inbox reports."
  (make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b))
              (funcall plan turns (length (%reports turns))))))

(defun %spawn-args (agent prompt)
  (format nil "{\"agent\":~s,\"prompt\":~s}" agent prompt))

(defun %one-spawn-plan (turns reports)
  (let ((last (%last-turn turns)))
    (cond
      ((plusp reports) (%say (format nil "parent got: ~a" (turn-text last))))
      ((eq :tool (llm-turn-role last)) (%say "waiting"))
      (t (%call "c1" "subagent" (%spawn-args "helper" "dig"))))))

(deftest subagent-reports-to-the-parent-inbox
  (with-agent-loop
    (let* ((helper (make-ai-agent :name "helper" :backend (%slow-echo 0.2)
                                  :instructions "Research."))
           (events (list nil))
           (parent (make-ai-agent :name "lead"
                                  :backend (%parent-backend #'%one-spawn-plan)
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go" :on-event (%events-into events)))
           (task (find-subagent-task run "sub-1")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "Started subagent helper with id sub-1. Do not poll or wait; its result arrives in your inbox."
                 (agent-invocation-result (first (agent-run-invocations run))))
          "the tool returns the id at once")
      (ok (find :waiting (car events) :key #'car) "the parent waited for its child")
      (ok (equal "parent got: <subagent id=\"sub-1\" name=\"helper\">echo: dig</subagent>"
                 (agent-run-text run)))
      (ok (= 1 (length (%reports (agent-run-turns run)))))
      (ok (eq :idle (subagent-task-status task)))
      (ok (eq run (agent-run-parent (subagent-task-run task))))
      (ok (find :inbox (car events) :key #'car)))))

(deftest subagent-continues-by-id
  (with-agent-loop
    (let* ((helper (make-ai-agent :name "helper" :backend (%slow-echo 0.05)))
           (plan (lambda (turns reports)
                   (let ((last (%last-turn turns)))
                     (cond
                       ((eq :tool (llm-turn-role last)) (%say "waiting"))
                       ((= reports 0) (%call "c1" "subagent" (%spawn-args "helper" "dig")))
                       ((= reports 1)
                        (%call "c2" "subagent"
                               "{\"id\":\"sub-1\",\"prompt\":\"dig deeper\"}"))
                       (t (%say (turn-text last)))))))
           (parent (make-ai-agent :name "lead" :backend (%parent-backend plan)
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go"))
           (task (find-subagent-task run "sub-1")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (search "Continued subagent sub-1"
                  (agent-invocation-result (second (agent-run-invocations run)))))
      (ok (equal "<subagent id=\"sub-1\" name=\"helper\">echo: dig deeper</subagent>"
                 (agent-run-text run)))
      (ok (= 1 (length (agent-run-subagents run))) "same child, no new one")
      (ok (= 2 (count :user (agent-run-turns (subagent-task-run task))
                      :key #'llm-turn-role))
          "the child kept its history and got the follow-up"))))

(deftest subagent-message-to-a-busy-child-is-queued
  (with-agent-loop
    (let* ((parent-run nil)
           (helper (make-ai-agent :name "helper" :backend (%slow-echo 0.3)))
           (parent (make-ai-agent :name "lead"
                                  :backend (%parent-backend #'%one-spawn-plan)
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go"
                              :on-event (lambda (kind payload)
                                          (case kind
                                            (:started (setf parent-run payload))
                                            (:waiting
                                             (ok (eq :queued
                                                     (continue-subagent
                                                      (find-subagent-task parent-run "sub-1")
                                                      "also this"))))))))
           (child (subagent-task-run (find-subagent-task run "sub-1"))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (find "also this" (agent-run-turns child) :key #'turn-text :test #'equal)
          "the queued message reached the child before its next generate"))))

(deftest subagent-cannot-spawn-subagents
  (with-agent-loop
    (let* ((grandchild (make-ai-agent :name "deep"))
           (child-backend
             (make-mock-llm-backend
              :handler (lambda (b turns &key &allow-other-keys)
                         (declare (ignore b))
                         (if (eq :tool (llm-turn-role (%last-turn turns)))
                             (%say (turn-text (%last-turn turns)))
                             (%call "g1" "subagent" (%spawn-args "deep" "x"))))))
           (helper (make-ai-agent :name "helper" :backend child-backend
                                  :tools (list (make-subagent-tool (list grandchild)))))
           (parent (make-ai-agent :name "lead"
                                  :backend (%parent-backend #'%one-spawn-plan)
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go"))
           (child (subagent-task-run (find-subagent-task run "sub-1")))
           (inv (first (agent-run-invocations child))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (eq :error (agent-invocation-status inv)))
      (ok (search "cannot start subagents of its own" (agent-invocation-result inv)))
      (ok (null (agent-run-subagents child))))))

(deftest subagent-tool-input-errors-reach-the-model
  (with-agent-loop
    (let* ((helper (make-ai-agent :name "helper"))
           (backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "subagent"
                                      :arguments (%spawn-args "nobody" "x"))
                                     (make-llm-tool-call-part
                                      :id "c2" :name "subagent"
                                      :arguments "{\"id\":\"sub-9\",\"prompt\":\"x\"}"))
                               "ok")))
           (parent (make-ai-agent :name "lead" :backend backend
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go"))
           (invs (agent-run-invocations run)))
      (ok (search "unknown agent \"nobody\"; choose one of: helper"
                  (agent-invocation-result (first invs))))
      (ok (search "Fix the arguments" (agent-invocation-result (first invs))))
      (ok (search "no subagent with id \"sub-9\"" (agent-invocation-result (second invs)))))))

(deftest subagent-tool-describes-the-inbox
  (let ((tool (make-subagent-tool (list (make-ai-agent :name "helper"
                                                       :instructions "Finds things.")))))
    (ok (search "Do not poll" (llm-tool-description tool)))
    (ok (search "arrives in your inbox" (llm-tool-description tool)))
    (ok (search "- helper: Finds things." (llm-tool-description tool)))
    (ok (equal '("helper")
               (tool-permission-resources tool "subagent" (%spawn-args "helper" "x"))))))

(deftest subagent-permission-rules-pick-agents
  (with-agent-loop
    (let* ((helper (make-ai-agent :name "helper"))
           (parent (make-ai-agent :name "lead"
                                  :backend (%parent-backend #'%one-spawn-plan)
                                  :permissions '(("subagent" "helper" :deny))
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (eq :denied (agent-invocation-status (first (agent-run-invocations run)))))
      (ok (null (agent-run-subagents run))))))

(deftest canceling-the-parent-cancels-its-subagents
  (with-agent-loop
    (let* ((parent-run nil)
           (helper (make-ai-agent :name "helper" :backend (%slow-echo 0.3)))
           (parent (make-ai-agent :name "lead"
                                  :backend (%parent-backend #'%one-spawn-plan)
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go"
                              :on-event (lambda (kind payload)
                                          (case kind
                                            (:started (setf parent-run payload))
                                            (:waiting (cancel-agent-run parent-run))))))
           (task (find-subagent-task run "sub-1")))
      (ok (eq :canceled (agent-run-finish-reason run)))
      (ok (eq :canceled (subagent-task-status task)))
      (ok (agent-run-handle-canceled-p (agent-run-handle (subagent-task-run task))))
      (ok (null (%reports (agent-run-turns run))) "no report from a canceled child"))))

(deftest subagents-on-finish-cancel
  (with-agent-loop
    (let* ((helper (make-ai-agent :name "helper" :backend (%slow-echo 0.3)))
           (parent (make-ai-agent :name "lead"
                                  :backend (%parent-backend #'%one-spawn-plan)
                                  :settings (make-agent-settings
                                             :extra (list :subagents-on-finish :cancel))
                                  :tools (list (make-subagent-tool (list helper)))))
           (run (run-ai-agent parent "go")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "waiting" (agent-run-text run)))
      (ok (eq :canceled (subagent-task-status (find-subagent-task run "sub-1")))))))
