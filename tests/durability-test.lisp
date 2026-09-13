(in-package #:ai-agent-protocol/tests)

(defun %counting-handler (generate-count responses)
  "RESPONSES is a list of LLM-RESPONSE. Extra calls reuse the last one."
  (lambda (backend turns &key &allow-other-keys)
    (declare (ignore backend turns))
    (incf (car generate-count))
    (let ((i (1- (car generate-count))))
      (if (< i (length responses))
          (elt responses i)
          (car (last responses))))))

(defun %tool-response (id name &optional (arguments "{}"))
  (make-llm-response
   :parts (list (make-llm-tool-call-part :id id :name name :arguments arguments))
   :finish-reason :tool-use))

(defun %text-response (text)
  (make-llm-response
   :parts (list (make-llm-text-part :text text))
   :finish-reason :stop))

(deftest durability-generate-only-replay
  (with-agent-loop
    (let* ((n (list 0))
           (backend (make-mock-llm-backend
                     :handler (%counting-handler n (list (%text-response "hi")))))
           (agent (make-ai-agent :name "echo" :backend backend))
           (journal (task-protocol:make-in-memory-journal))
           (dur (ai-agent-protocol/durability:make-agent-durability
                 :journal journal :task-id "gen-only"))
           (run (run-ai-agent agent "hello" :durability dur)))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "hi" (agent-run-text run)))
      (ok (= 1 (car n)))
      (let* ((journal2 (task-protocol:copy-in-memory-journal journal))
             (dur2 (ai-agent-protocol/durability:make-agent-durability
                    :journal journal2 :task-id "gen-only"))
             (run2 (run-ai-agent agent "hello" :durability dur2)))
        (ok (eq :stop (agent-run-finish-reason run2)))
        (ok (equal "hi" (agent-run-text run2)))
        (ok (= 1 (car n)) "replay must not call generate again")))))

(deftest durability-kill-and-replay-generate-tool-hitl
  (with-agent-loop
    (let* ((n (list 0))
           (sum-n (list 0))
           (danger-n (list 0))
           (backend (make-mock-llm-backend
                     :handler (%counting-handler
                               n
                               (list (%tool-response "c1" "sum")
                                     (%tool-response "c2" "danger")
                                     (%text-response "done")))))
           (agent (make-ai-agent :name "gated" :backend backend))
           (journal (task-protocol:make-in-memory-journal)))
      (define-agent-tool agent "sum" (:description "add") (args)
        (declare (ignore args))
        (incf (car sum-n))
        "3")
      (register-agent-tool
       agent
       (make-function-tool :name "danger" :approval-required-p t
                           :handler (lambda (args)
                                      (declare (ignore args))
                                      (incf (car danger-n))
                                      "did-it")))
      (let* ((dur (ai-agent-protocol/durability:make-agent-durability
                   :journal journal :task-id "hitl-run"))
             (run (run-ai-agent agent "go" :durability dur)))
        (ok (eq :approval (agent-run-finish-reason run)))
        (ok (= 2 (car n)))
        (ok (= 1 (car sum-n)))
        (ok (= 0 (car danger-n)))
        (ok (= 1 (length (agent-run-pending run))))
        (let ((waiting
               (find-if (lambda (e) (typep e 'task-protocol:wait-input))
                        (task-protocol:journal-events
                         journal
                         (ai-agent-protocol/durability:agent-durability-task dur)))))
          (ok waiting "HITL is journaled as wait-input"))
        (let* ((journal2 (task-protocol:copy-in-memory-journal journal))
               (dur2 (ai-agent-protocol/durability:make-agent-durability
                      :journal journal2 :task-id "hitl-run"))
               (run2 (run-ai-agent agent "go" :durability dur2)))
          (ok (eq :approval (agent-run-finish-reason run2)))
          (ok (= 2 (car n)) "replay skips completed generate steps")
          (ok (= 1 (car sum-n)) "replay skips the completed tool")
          (ok (= 0 (car danger-n)))
          (ok (= 1 (length (agent-run-pending run2))))
          (approve-invocation run2 (first (agent-run-pending run2)))
          (let ((run3 (resume-ai-agent run2)))
            (ok (eq :stop (agent-run-finish-reason run3)))
            (ok (equal "done" (agent-run-text run3)))
            (ok (= 3 (car n)))
            (ok (= 1 (car sum-n)))
            (ok (= 1 (car danger-n)))
            (ok (equal "did-it"
                       (agent-invocation-result
                        (find "danger" (agent-run-invocations run3)
                              :key #'agent-invocation-name
                              :test #'equal))))))))))

(deftest-parametrize durability-coerce
    ((kind)
     (:auto)
     (:plist))
  (with-agent-loop
    (let* ((n (list 0))
           (backend (make-mock-llm-backend
                     :handler (%counting-handler n (list (%text-response "ok")))))
           (agent (make-ai-agent :name "echo" :backend backend))
           (durability (if (eq kind :auto)
                           t
                           (list :journal (task-protocol:make-in-memory-journal)
                                 :task-id "from-plist")))
           (run (run-ai-agent agent "hi" :durability durability)))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "ok" (agent-run-text run)))
      (ok (ai-agent-protocol/durability:agent-durability-p
           (agent-run-durability run)))
      (ok (= 1 (car n))))))
