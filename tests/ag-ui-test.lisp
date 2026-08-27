(in-package #:ai-agent-protocol/tests)

(deftest ag-ui-handler-emits-run-events
  (with-agent-loop
    (let* ((agent (make-ai-agent :name "echo"
                                 :backend (make-mock-llm-backend)))
           (handler (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler agent))
           (input (ag-ui-protocol:make-run-agent-input
                   :thread-id "t1" :run-id "r1"
                   :messages (list (ag-ui-protocol:make-ag-ui-message
                                    :id "m1" :role "user" :content "hi"))))
           (events (funcall handler input))
           (types (mapcar #'ag-ui-protocol:ag-ui-event-type events)))
      (ok (equal "RUN_STARTED" (first types)))
      (ok (find "TEXT_MESSAGE_CONTENT" types :test #'equal))
      (ok (equal "RUN_FINISHED" (car (last types)))))))
