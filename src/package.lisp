(defpackage #:ai-agent-protocol
  (:use #:cl #:llm-protocol)
  (:nicknames #:stack-ai-agent)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:event #:event-protocol)
                    (#:conversation #:conversation-protocol)
                    (#:steer #:steer-protocol))
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
   #:tool-failure
   #:tool-failure-message
   #:tool-input-error
   #:tool-unavailable
   #:tool-timeout
   #:tool-failure-kind
   #:tool-failure-text
   #:tool-permission-denied
   #:tool-permission-denied-action
   #:tool-permission-denied-resources
   #:tool-permission-denied-call-resources
   #:tool-permission-denied-feedback
   #:tool-permission-denied-rule
   #:tool-permission-denied-decision

   #:permission-effect
   #:permission-rule
   #:make-permission-rule
   #:permission-rule-p
   #:permission-rule-action
   #:permission-rule-resource
   #:permission-rule-effect
   #:permission-rule-feedback
   #:coerce-permission-rule
   #:coerce-permission-rules
   #:permission-glob-match-p
   #:permission-rule-matches-p
   #:permission-rules-effect
   #:tool-permission-resources
   #:*permission-policy*
   #:permission-policy-effect
   #:agent-permission
   #:permission-rule-store
   #:save-permission-rule
   #:list-permission-rules
   #:in-memory-permission-rule-store
   #:make-in-memory-permission-rule-store
   #:tool-permission-always-resources

   #:subagent-tool
   #:make-subagent-tool
   #:subagent-tool-p
   #:subagent-tool-agents
   #:+subagent-tool-description+
   #:subagent-task
   #:subagent-task-id
   #:subagent-task-name
   #:subagent-task-agent
   #:subagent-task-parent
   #:subagent-task-run
   #:subagent-task-status
   #:subagent-task-restored-p
   #:+lost-subagent-text+
   #:subagent-task-result
   #:find-subagent-task
   #:start-subagent
   #:continue-subagent
   #:continue-ai-agent-async
   #:continue-ai-agent
   #:agent-run-post
   #:agent-run-inbox
   #:drain-agent-run-inbox
   #:agent-run-subagents
   #:agent-run-instructions
   #:tool-instruction-paths
   #:*instruction-path-keys*
   #:agent-instructions-turn
   #:agent-run-parent
   #:+max-steps-prompt+
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
   #:invoke-approve-always
   #:invoke-deny-all
   #:agent-permission-save-error
   #:agent-permission-save-error-invocation
   #:agent-permission-save-error-rule
   #:agent-permission-save-error-cause
   #:invoke-skip-save
   #:invoke-approve-once
   #:denial-decision
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
   #:ai-agent-model
   #:ai-agent-name
   #:ai-agent-memory
   #:ai-agent-session
   #:ai-agent-steering
   #:ai-agent-permissions
   #:ai-agent-permission-store
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
   #:agent-settings-model
   #:coerce-agent-settings

   #:function-tool
   #:make-function-tool
   #:function-tool-p
   #:function-tool-handler
   #:function-tool-approval-required-p
   #:function-tool-async-p
   #:function-tool-permission-resources
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
   #:agent-run-segment
   #:agent-run-finish-reason
   #:agent-run-last-response
   #:agent-run-text
   #:agent-run-handle
   #:agent-run-on-event
   #:agent-run-on-part
   #:agent-run-in-flight-turn
   #:agent-run-session
   #:agent-run-durability
   #:agent-run-approved-permissions

   #:agent-run-handle
   #:agent-run-handle-p
   #:agent-run-handle-canceled-p

   #:list-agent-tools
   #:collect-run-tools
   #:prepare-agent-turns
   #:skill-tool-source
   #:make-skill-tool-source
   #:skill-tool-source-skill
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
   #:complete-invocation

   #:*off-loop-specials*

   ;; 0.15.0 (#37): tool-source combinators (src/toolsets.lisp)
   #:wrapper-tool-source #:make-wrapper-tool-source #:tool-source-inner
   #:combined-tool-source #:make-combined-tool-source #:combined-tool-source-sources
   #:prefixed-tool-source #:make-prefixed-tool-source #:prefixed-tool-source-prefix
   #:renamed-tool-source #:make-renamed-tool-source #:renamed-tool-source-names
   #:filtered-tool-source #:make-filtered-tool-source #:filtered-tool-source-predicate
   #:prepared-tool-source #:make-prepared-tool-source #:prepared-tool-source-function
   #:approval-required-tool-source #:make-approval-required-tool-source
   #:approval-required-tool-source-predicate
   #:tool-predicate-p
   #:tool-name-conflict #:tool-name-conflict-name #:tool-name-conflict-sources
   #:agent-tool-output #:make-agent-tool-output #:agent-tool-output-text
   #:agent-tool-output-parts #:agent-tool-output-structured
   #:agent-invocation-parts #:agent-invocation-structured))

(in-package #:ai-agent-protocol)
