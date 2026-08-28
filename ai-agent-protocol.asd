(defsystem "ai-agent-protocol"
  :version "0.1.1"
  :description "Async-first CLOS agent protocol over llm-protocol (CL tools, invocations, approvals)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "event-protocol" "bordeaux-threads")
  :properties (:cl-repo
               (:ci (:with ("ai-agent-protocol/mcp"
                            "ai-agent-protocol/ag-ui"
                            "ai-agent-protocol/a2a"
                            "event-backend-libuv"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "async")
               (:file "protocol")
               (:file "loop"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/mcp"
  :version "0.1.0"
  :description "mcp-protocol tool source + sampling handler for ai-agent-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("ai-agent-protocol" "mcp-protocol")
  :serial t
  :pathname "src/mcp"
  :components ((:file "package")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "ai-agent-protocol/tests"))))

(defsystem "ai-agent-protocol/ag-ui"
  :version "0.1.0"
  :description "Expose an ai-agent via ag-ui-protocol"
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

(defsystem "ai-agent-protocol/tests"
  :depends-on ("ai-agent-protocol"
               "ai-agent-protocol/mcp"
               "ai-agent-protocol/ag-ui"
               "ai-agent-protocol/a2a"
               "event-backend-libuv"
               "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "restarts-test")
               (:file "mcp-test")
               (:file "ag-ui-test")
               (:file "a2a-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
