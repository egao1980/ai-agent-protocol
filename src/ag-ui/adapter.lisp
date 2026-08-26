(in-package #:ai-agent-protocol/ag-ui)

;;; AG-UI run-agent is sync and returns an event list. We await the async loop.

(defun make-ai-agent-ag-ui-handler (agent &key settings)
  "Handler for MAKE-AG-UI-AGENT. Requires a bound event loop."
  (lambda (input)
    (let* ((thread (or (ag-ui-protocol:run-agent-input-thread-id input) "thread"))
           (run-id (or (ag-ui-protocol:run-agent-input-run-id input) "run"))
           (text (ag-ui-protocol:last-user-text input))
           (ar (run-ai-agent agent text :settings settings))
           (mid "msg-agent")
           (out (or (agent-run-text ar) ""))
           (events (list (ag-ui-protocol:make-run-started-event
                          :thread-id thread :run-id run-id))))
      (when (plusp (length out))
        (setf events
              (append events
                      (list (ag-ui-protocol:make-text-message-start-event
                             :message-id mid :role "assistant")
                            (ag-ui-protocol:make-text-message-content-event
                             :message-id mid :delta out)
                            (ag-ui-protocol:make-text-message-end-event
                             :message-id mid)))))
      (dolist (inv (agent-run-invocations ar))
        (let ((id (agent-invocation-id inv)))
          (setf events
                (append events
                        (list (ag-ui-protocol:make-tool-call-start-event
                               :tool-call-id id
                               :tool-call-name (agent-invocation-name inv))
                              (ag-ui-protocol:make-tool-call-args-event
                               :tool-call-id id
                               :delta (agent-invocation-arguments inv))
                              (ag-ui-protocol:make-tool-call-end-event
                               :tool-call-id id)
                              (ag-ui-protocol:make-tool-call-result-event
                               :tool-call-id id
                               :content (or (agent-invocation-result inv) "")))))))
      (append events
              (list (ag-ui-protocol:make-run-finished-event
                     :thread-id thread :run-id run-id))))))
