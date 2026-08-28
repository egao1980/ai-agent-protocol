(defpackage #:ai-agent-protocol
  (:use #:cl #:llm-protocol)
  (:nicknames #:stack-ai-agent)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:event #:event-protocol))
  (:export
   #:agent-condition
   #:agent-error
   #:agent-error-message
   #:agent-error-run
   #:agent-missing-loop
   #:agent-timeout
   #:agent-canceled
   #:agent-unknown-tool
   #:agent-unknown-tool-name
   #:agent-tool-error
   #:agent-tool-error-invocation
   #:agent-tool-error-cause
   #:agent-tool-error-name
   #:agent-generate-error
   #:agent-generate-error-step
   #:agent-generate-error-cause
   #:agent-max-steps
   #:agent-approval-required
   #:agent-approval-required-invocation
   #:call-with-agent-restarts
   #:with-agent-restarts
   #:invoke-retry
   #:invoke-use-value
   #:invoke-skip-tool
   #:invoke-approve
   #:invoke-deny
   #:invoke-pause-for-approval
   #:auto-skip-tool
   #:auto-pause-approval
   #:auto-retry
   #:with-auto-skip-tool
   #:with-auto-pause-approval

   #:ai-agent
   #:ai-agent-p
   #:make-ai-agent
   #:ai-agent-backend
   #:ai-agent-instructions
   #:ai-agent-tools
   #:ai-agent-handoffs
   #:ai-agent-settings
   #:ai-agent-name
   #:register-agent-handoff
   #:unregister-agent-handoff
   #:defagent

   #:agent-settings
   #:make-agent-settings
   #:agent-settings-p
   #:agent-settings-llm
   #:agent-settings-max-steps
   #:agent-settings-timeout
   #:agent-settings-extra
   #:coerce-agent-settings

   #:function-tool
   #:make-function-tool
   #:function-tool-p
   #:function-tool-handler
   #:function-tool-approval-required-p
   #:function-tool-async-p
   #:register-agent-tool
   #:unregister-agent-tool
   #:find-agent-tool
   #:define-agent-tool

   #:agent-invocation
   #:make-agent-invocation
   #:agent-invocation-p
   #:agent-invocation-id
   #:agent-invocation-name
   #:agent-invocation-arguments
   #:agent-invocation-status
   #:agent-invocation-result
   #:agent-invocation-source
   #:agent-invocation-error-p

   #:agent-run
   #:make-agent-run
   #:agent-run-p
   #:agent-run-agent
   #:agent-run-turns
   #:agent-run-invocations
   #:agent-run-pending
   #:agent-run-step
   #:agent-run-finish-reason
   #:agent-run-last-response
   #:agent-run-text
   #:agent-run-handle
   #:agent-run-on-event
   #:agent-run-on-part
   #:agent-run-in-flight-turn

   #:agent-run-handle
   #:agent-run-handle-p
   #:agent-run-handle-canceled-p

   #:list-agent-tools
   #:collect-run-tools
   #:prepare-agent-turns
   #:agent-approve-p
   #:tool-executable-p
   #:tool-needs-approval-p
   #:invoke-tool-async
   #:invoke-tool
   #:cancel-agent-run

   #:run-ai-agent-async
   #:run-ai-agent
   #:resume-ai-agent-async
   #:resume-ai-agent
   #:approve-invocation
   #:deny-invocation
   #:complete-invocation))

(in-package #:ai-agent-protocol)
