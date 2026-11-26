(defsystem "ai-agent-protocol"
  :version "0.16.1"
  :description "Async-first CLOS agent protocol over llm-protocol (CL tools, invocations, approvals)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "llm-protocol" "0.7.0") "event-protocol" "bordeaux-threads"
               (:version "conversation-protocol" "0.3.0") (:version "steer-protocol" "0.3.0"))
  :properties (:cl-repo
               (:ci (:with ("ai-agent-protocol/mcp"
                            "ai-agent-protocol/policy"
                            "ai-agent-protocol/ag-ui"
                            "ai-agent-protocol/a2a"
                            "ai-agent-protocol/durability"
                            "ai-agent-protocol/telemetry"
                            "event-backend-libuv"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "async")
               (:file "protocol")
               ;; 0.15.0 (#37)
               (:file "toolsets")
               (:file "permissions")
               (:file "instructions")
               (:file "loop")
               (:file "subagents")
               (:file "journal")
               (:file "skill-source")
               (:file "resilience"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/mcp"
  :version "0.2.0"
  :description "mcp-protocol tool source + sampling handler for ai-agent-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "ai-agent-protocol" "0.15.0") (:version "mcp-protocol" "0.4.0")
               "bordeaux-threads")
  :serial t
  :pathname "src/mcp"
  :components ((:file "package")
               (:file "adapter")
               ;; 0.2.0 (#37)
               (:file "source"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/policy"
  :version "0.15.0"
  :description "policy-protocol policies as tool-source predicates (filters and approvals)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "ai-agent-protocol" "0.15.0") (:version "policy-protocol" "0.1.0"))
  :serial t
  :pathname "src/policy"
  :components ((:file "package")
               (:file "policy"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/ag-ui"
  :version "0.3.0"
  :description "Streaming encoder from ai-agent on-event to AG-UI events"
  :author "egao1980"
  :license "MIT"
  :depends-on ("ai-agent-protocol" "ag-ui-protocol")
  :serial t
  :pathname "src/ag-ui"
  :components ((:file "package")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/a2a"
  :version "0.1.0"
  :description "Expose an ai-agent via a2a-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("ai-agent-protocol" "a2a-protocol")
  :serial t
  :pathname "src/a2a"
  :components ((:file "package")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/durability"
  :version "0.4.0"
  :description "task-protocol journal for ai-agent-protocol (:durability)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("ai-agent-protocol" "task-protocol")
  :serial t
  :pathname "src/durability"
  :components ((:file "package")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/telemetry"
  :version "0.1.0"
  :description "GenAI semconv spans per agent run and tool invocation"
  :author "egao1980"
  :license "MIT"
  :depends-on ("ai-agent-protocol" "telemetry-protocol")
  :serial t
  :pathname "src/telemetry"
  :components ((:file "package")
               (:file "instrument"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/tests"
  :depends-on ("ai-agent-protocol"
               "ai-agent-protocol/mcp"
               ;; 0.15.0 (#37)
               "ai-agent-protocol/policy"
               (:version "logic-backend-datalog" "0.1.0")
               (:version "rpc-backend-stdio" "0.3.2")
               "mcp-protocol"
               "ai-agent-protocol/ag-ui"
               "ai-agent-protocol/a2a"
               "ai-agent-protocol/durability"
               "ai-agent-protocol/telemetry"
               "task-protocol"
               (:version "conversation-protocol" "0.4.0")
               "conversation-protocol/summary"
               "event-backend-libuv"
               "json-backend-jzon"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "restarts-test")
               (:file "mcp-test")
               (:file "ag-ui-test")
               (:file "a2a-test")
               (:file "telemetry-test")
               (:file "durability-test")
               (:file "harness-test")
               (:file "permissions-test")
               (:file "subagents-test")
               (:file "instructions-test")
               (:file "durable-state-test")
               (:file "durable-continue-test")
               (:file "permission-approvals-test")
               (:file "ag-ui-approvals-test")
               (:file "denial-model-test")
               (:file "cancel-continue-test")
               (:file "denied-steering-test")
               ;; 0.15.0 (#37)
               (:file "toolsets-test")
               (:file "mcp-source-test")
               ;; 0.16.0 (#39)
               (:file "resilience-test")
               ;; 0.16.1 (#51)
               (:file "tool-end-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

;; Thin alias. durability-test now runs in default test-op / :ci :with.
(defsystem "ai-agent-protocol/durability-tests"
  :depends-on ("ai-agent-protocol/tests")
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))
