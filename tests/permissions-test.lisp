(in-package #:ai-agent-protocol/tests)

;;; Permission rules: {action, resource, effect}, last match wins, deny on any
;;; resource wins, a policy layer only tightens, :ask is the approval pause and
;;; a deny (or rejected ask) with feedback is a tool error the model reads.

(deftest permission-glob-matching
  (ok (permission-glob-match-p "*" ""))
  (ok (permission-glob-match-p "*" "src/a/b.lisp") "* crosses /")
  (ok (permission-glob-match-p "src/*" "src/a/b.lisp"))
  (ng (permission-glob-match-p "src/*" "test/a.lisp"))
  (ok (permission-glob-match-p "*.lisp" "src/a.lisp"))
  (ng (permission-glob-match-p "*.lisp" "src/a.lisp.bak") "whole value")
  (ok (permission-glob-match-p "a?c" "abc"))
  (ng (permission-glob-match-p "a?c" "ac"))
  (ok (permission-glob-match-p "git status *" "git status --short"))
  (ok (permission-glob-match-p "git status *" "git status") "trailing \" *\" matches the bare command")
  (ng (permission-glob-match-p "git status *" "git statusx"))
  (ok (permission-glob-match-p "src/*" "src\\win\\path") "backslashes are slashes")
  (ok (permission-glob-match-p "*a" "*ba")))

(defun %rules (&rest specs)
  (coerce-permission-rules specs))

(deftest permission-rules-last-match-wins
  (let ((rules (%rules '("*" "*" :ask)
                       '("bash" "*" :deny)
                       '("bash" "git *" :allow))))
    (ok (eq :allow (permission-rules-effect rules "bash" '("git log"))))
    (ok (eq :deny (permission-rules-effect rules "bash" '("rm -rf /"))))
    (ok (eq :ask (permission-rules-effect rules "edit" '("x")))))
  (testing "a deny on any resource wins, then an ask"
    (let ((rules (%rules '("edit" "*" :allow)
                         '("edit" "secret/*" :deny)
                         '("edit" "docs/*" :ask))))
      (ok (eq :deny (permission-rules-effect rules "edit" '("src/a" "secret/k" "docs/x"))))
      (ok (eq :ask (permission-rules-effect rules "edit" '("src/a" "docs/x"))))
      (ok (eq :allow (permission-rules-effect rules "edit" '("src/a" "src/b"))))))
  (testing "a resource no rule matches leaves the decision to the default"
    (let ((rules (%rules '("edit" "src/*" :allow))))
      (ok (null (permission-rules-effect rules "edit" '("src/a" "lib/b"))))
      (ok (null (permission-rules-effect rules "read" '("src/a"))))))
  (ok (signals (make-permission-rule "x" "*" :maybe) 'type-error)))

(defun %edit-agent (box &key permissions settings (paths '("src/a.lisp"))
                          approval)
  "Agent whose model calls edit once; the tool reports PATHS as resources."
  (let* ((backend (make-mock-llm-backend
                   :handler (%one-shot-tools
                             (list (make-llm-tool-call-part
                                    :id "c1" :name "edit" :arguments "{}"))
                             "ok")))
         (agent (make-ai-agent :name "coder" :backend backend
                               :permissions permissions
                               :settings settings)))
    (register-agent-tool
     agent
     (make-function-tool :name "edit"
                         :approval-required-p approval
                         :permission-resources (lambda (args)
                                                 (declare (ignore args))
                                                 paths)
                         :handler (lambda (args)
                                    (declare (ignore args))
                                    (setf (car box) t)
                                    "edited")))
    agent))

(defun %first-invocation (run)
  (first (agent-run-invocations run)))

(deftest permission-deny-is-a-tool-error-with-feedback
  (with-agent-loop
    (let* ((ran (list nil))
           (events (list nil))
           (agent (%edit-agent
                   ran
                   :paths '("src/a.lisp" "secret/key.pem")
                   :permissions (list '("edit" "*" :allow)
                                      (make-permission-rule
                                       "edit" "secret/*" :deny
                                       :feedback "Secrets are off limits; edit files under src/ only."))))
           (run (run-ai-agent agent "go" :on-event (%events-into events)))
           (inv (%first-invocation run)))
      (ok (eq :stop (agent-run-finish-reason run)) "the run goes on")
      (ok (null (car ran)) "the tool did not run")
      (ok (eq :denied (agent-invocation-status inv)))
      (ok (agent-invocation-error-p inv))
      (ok (search "Secrets are off limits" (agent-invocation-result inv)))
      (ok (search "secret/key.pem" (agent-invocation-result inv)))
      (ok (search "Do not repeat the same call" (agent-invocation-result inv)))
      (let ((perm (find :permission (car events) :key #'car)))
        (ok perm)
        (ok (typep (getf (cdr perm) :condition) 'tool-permission-denied)))
      (ok (find-if (lambda (turn)
                     (and (eq :tool (llm-turn-role turn))
                          (llm-tool-result-part-error-p (first (llm-turn-parts turn)))))
                   (agent-run-turns run))
          "the model sees an error tool result"))))

(deftest permission-run-settings-layer-comes-last
  (with-agent-loop
    (let* ((ran (list nil))
           (agent (%edit-agent ran
                               :permissions '(("edit" "*" :deny))
                               :settings (make-agent-settings
                                          :extra (list :permissions
                                                       '(("edit" "src/*" :allow)))))))
      (let ((run (run-ai-agent agent "go")))
        (ok (car ran))
        (ok (eq :done (agent-invocation-status (%first-invocation run))))))))

(deftest permission-ask-pauses-and-a-rejection-carries-feedback
  (with-agent-loop
    (let* ((ran (list nil))
           (agent (%edit-agent ran :permissions '(("edit" "*" :ask))))
           (run (run-ai-agent agent "go")))
      (ok (eq :approval (agent-run-finish-reason run)) ":ask is the approval pause")
      (deny-invocation run (first (agent-run-pending run))
                       :feedback "Ask me before touching src/a.lisp.")
      (let* ((run2 (resume-ai-agent run))
             (inv (%first-invocation run2)))
        (ok (eq :stop (agent-run-finish-reason run2)))
        (ok (null (car ran)))
        (ok (agent-invocation-error-p inv))
        (ok (search "rejected" (agent-invocation-result inv)))
        (ok (search "Ask me before touching" (agent-invocation-result inv)))))))

(deftest permission-deny-restart-takes-feedback
  (with-agent-loop
    (let* ((ran (list nil))
           (agent (%edit-agent ran :approval t))
           (run (handler-bind ((agent-approval-required
                                 (lambda (c) (invoke-deny c "Use the read tool instead."))))
                  (run-ai-agent agent "go")))
           (inv (%first-invocation run)))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (null (car ran)))
      (ok (search "Use the read tool instead." (agent-invocation-result inv))))))

(deftest permission-rule-overrides-the-tool-default
  (with-agent-loop
    (let* ((ran (list nil))
           (agent (%edit-agent ran :approval t :permissions '(("edit" "src/*" :allow)))))
      (ok (eq :stop (agent-run-finish-reason (run-ai-agent agent "go"))))
      (ok (car ran)))))

(deftest permission-policy-only-tightens
  (with-agent-loop
    (let ((ran (list nil)))
      (testing "policy turns allow into ask"
        (let ((agent (%edit-agent ran :permissions '(("edit" "*" :allow))
                                      :settings (make-agent-settings
                                                 :extra (list :permission-policy
                                                              '(("edit" "src/*" :ask)))))))
          (ok (eq :approval (agent-run-finish-reason (run-ai-agent agent "go"))))))
      (testing "policy allow cannot loosen a deny"
        (let* ((agent (%edit-agent ran :permissions '(("edit" "*" :deny))
                                       :settings (make-agent-settings
                                                  :extra (list :permission-policy
                                                               '(("*" "*" :allow))))))
               (run (run-ai-agent agent "go")))
          (ok (eq :denied (agent-invocation-status (%first-invocation run))))
          (ok (null (car ran)))))
      (testing "*permission-policy* function hard-denies"
        (let* ((*permission-policy* (lambda (action resources effect)
                                      (declare (ignore resources effect))
                                      (and (equal action "edit") :deny)))
               (agent (%edit-agent ran :permissions '(("edit" "*" :allow))))
               (run (run-ai-agent agent "go")))
          (ok (eq :denied (agent-invocation-status (%first-invocation run))))
          (ok (null (car ran)))))
      (testing "a policy returning allow leaves an ask alone"
        (let ((agent (%edit-agent ran :permissions '(("edit" "*" :ask))
                                      :settings (make-agent-settings
                                                 :extra (list :permission-policy
                                                              (lambda (a r e)
                                                                (declare (ignore a r e))
                                                                :allow))))))
          (ok (eq :approval (agent-run-finish-reason (run-ai-agent agent "go")))))))))

(deftest agent-permission-direct
  (let* ((tool (make-function-tool :name "bash"
                                   :permission-resources (lambda (args) (list args))))
         (agent (make-ai-agent :name "a" :permissions '(("bash" "*" :ask)
                                                        ("bash" "ls *" :allow)))))
    (ok (eq :allow (agent-permission agent tool "bash" "ls -la")))
    (ok (eq :ask (agent-permission agent tool "bash" "make")))
    (ok (agent-approve-p agent tool "bash" "make"))
    (ng (agent-approve-p agent tool "bash" "ls"))
    (ok (equal '("*") (tool-permission-resources (make-llm-tool :name "x") "x" "{}")))))
