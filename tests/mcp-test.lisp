(in-package #:ai-agent-protocol/tests)

(deftest mcp-sampling-json-params
  (let* ((backend (make-mock-llm-backend))
         (handler (ai-agent-protocol/mcp:make-mcp-sampling-handler :backend backend))
         (client (make-instance 'mcp-protocol:mcp-client :sampling-handler handler))
         (params (mcp-protocol:json-object
                  "messages" (vector (mcp-protocol:json-object
                                      "role" "user"
                                      "content" (mcp-protocol:make-text-content "ping")))
                  "maxTokens" 16
                  "modelPreferences" (mcp-protocol:json-object
                                      "hints" (vector (mcp-protocol:json-object
                                                       "name" "mock")))))
         (out (mcp-protocol:create-message client params)))
    (ok (equal "assistant" (gethash "role" out)))
    (ok (equal "echo: ping" (gethash "text" (gethash "content" out))))
    (ok (equal "endTurn" (gethash "stopReason" out)))
    (ok (equal "mock" (gethash "model" out)))))

(deftest mcp-sampling-request-object
  (let* ((backend (make-mock-llm-backend :prefix "s:"))
         (handler (ai-agent-protocol/mcp:make-mcp-sampling-handler :backend backend))
         (req (mcp-protocol:make-mcp-sampling-request
               (list (make-instance 'mcp-protocol:mcp-sampling-message
                                    :role "user"
                                    :content "pong"))
               :system-prompt "sys"
               :max-tokens 8))
         (out (funcall handler req)))
    (ok (equal "s:pong" (gethash "text" (gethash "content" out))))))

(deftest mcp-peer-as-tool-source
  (with-agent-loop
    (let* ((server (make-instance 'mcp-protocol:mcp-server))
           (src (ai-agent-protocol/mcp:make-mcp-tool-source server))
           (backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "echo" :arguments "{}"))
                               "done"))))
      (mcp-protocol:register-tool
       server
       (mcp-protocol:make-mcp-tool "echo"
                                   :description "echo"
                                   :handler (lambda (args)
                                              (declare (ignore args))
                                              "pong")))
      (let* ((agent (make-ai-agent :name "mcp-host" :backend backend
                                   :tools (list src)))
             (run (run-ai-agent agent "go")))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (equal "pong"
                   (agent-invocation-result
                    (first (agent-run-invocations run)))))))))
