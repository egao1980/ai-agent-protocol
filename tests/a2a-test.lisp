(in-package #:ai-agent-protocol/tests)

(deftest a2a-handler-completes-task
  (with-agent-loop
    (let* ((ai (make-ai-agent :name "echo"
                              :backend (make-mock-llm-backend)))
           (handler (ai-agent-protocol/a2a:a2a-ai-agent-handler ai))
           (msg (a2a-protocol:make-a2a-message
                 :parts (list (a2a-protocol:make-text-part "hi"))))
           (task (a2a-protocol:make-a2a-task :id "task-1"))
           (out (funcall handler nil msg task)))
      (ok (eq :completed (a2a-protocol:a2a-task-state out)))
      (ok (search "echo: hi"
                  (a2a-protocol:a2a-part-text
                   (first (a2a-protocol:a2a-artifact-parts
                           (first (a2a-protocol:a2a-task-artifacts out))))))))))
