(defpackage #:ai-agent-protocol/mcp
  (:use #:cl #:ai-agent-protocol #:llm-protocol)
  (:export #:make-mcp-sampling-handler
           #:llm-response->mcp-create-message
           #:mcp-tool-source
           #:make-mcp-tool-source
           #:mcp-tool-source-peer
           ;; 0.2.0 (#37)
           #:mcp-tool-source-cache-tools
           #:mcp-tool-source-tool-error-behavior
           #:mcp-tool-source-process-tool-call
           #:mcp-tool-source-timeout
           #:mcp-tool-source-list-timeout
           #:mcp-tool-source-tools
           #:mcp-tool-source-invalidate
           #:find-mcp-tool
           #:mcp-tool-source-read-only-p
           #:mcp-tool-source-output-schema
           #:mcp-result-output
           #:mcp-tool-result-error
           #:mcp-tool-result-error-name
           #:mcp-tool-result-error-result
           #:mcp-tool-result-error-retryable
           #:mcp-tool-run-error
           #:mcp-tool-run-error-name
           #:mcp-tool-run-error-result
           #:+tool-error-behaviors+))

(in-package #:ai-agent-protocol/mcp)
