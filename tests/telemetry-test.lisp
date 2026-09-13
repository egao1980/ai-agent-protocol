(in-package #:ai-agent-protocol/tests)

(defun %tel-attr (attrs key)
  (loop for (k v) on attrs by #'cddr
        when (equal k key)
          return v))

(defun %tel-span (spans name)
  (find name spans :key #'telemetry-protocol:telemetry-span-name :test #'equal))

(defmacro with-recording-agent-telemetry (&body body)
  `(let ((telemetry-protocol:*telemetry-backend*
           (telemetry-protocol:make-recording-telemetry-backend))
         (telemetry-protocol:*tracer-provider* nil)
         (telemetry-protocol:*current-span* nil)
         (telemetry-protocol:*current-trace-id* nil))
     ,@body))

(deftest run-span-records-agent-name
  (with-recording-agent-telemetry
    (with-agent-loop
      (let* ((backend (make-mock-llm-backend))
             (agent (make-ai-agent :name "echo" :backend backend
                                   :instructions "Be brief."))
             (run (run-ai-agent agent "hi"))
             (span (%tel-span (telemetry-protocol:recorded-spans
                               telemetry-protocol:*telemetry-backend*)
                              "invoke_agent"))
             (attrs (and span (telemetry-protocol:telemetry-span-attributes span))))
        (ok (equal "echo: hi" (agent-run-text run)))
        (ok span)
        (ok (equal "invoke_agent"
                   (%tel-attr attrs telemetry-protocol:+gen-ai-operation-name+)))
        (ok (equal "echo"
                   (%tel-attr attrs telemetry-protocol:+gen-ai-agent-name+)))
        (ok (equal "mock"
                   (%tel-attr attrs telemetry-protocol:+gen-ai-request-model+)))))))

(deftest tool-span-records-tool-name
  (with-recording-agent-telemetry
    (with-agent-loop
      (let* ((backend (make-mock-llm-backend
                       :handler (%one-shot-tools
                                 (list (make-llm-tool-call-part
                                        :id "c1" :name "sum" :arguments "{}"))
                                 "3")))
             (agent (make-ai-agent :name "math" :backend backend)))
        (define-agent-tool agent "sum" (:description "add") (args)
          (declare (ignore args))
          "3")
        (let ((run (run-ai-agent agent "1+2")))
          (ok (equal "3" (agent-run-text run)))
          (let* ((spans (telemetry-protocol:recorded-spans
                         telemetry-protocol:*telemetry-backend*))
                 (run-span (%tel-span spans "invoke_agent"))
                 (tool (%tel-span spans "execute_tool"))
                 (attrs (and tool (telemetry-protocol:telemetry-span-attributes tool))))
            (ok run-span)
            (ok tool)
            (ok (equal "execute_tool"
                       (%tel-attr attrs telemetry-protocol:+gen-ai-operation-name+)))
            (ok (equal "sum"
                       (%tel-attr attrs telemetry-protocol:+gen-ai-tool-name+)))
            (ok (equal (telemetry-protocol:telemetry-span-trace-id run-span)
                       (telemetry-protocol:telemetry-span-trace-id tool)))
            (ok (equal (telemetry-protocol:telemetry-span-id run-span)
                       (telemetry-protocol:telemetry-span-parent-id tool)))))))))
