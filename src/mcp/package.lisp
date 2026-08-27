(defpackage #:ai-agent-protocol/mcp
  (:use #:cl #:ai-agent-protocol #:llm-protocol)
  (:export #:make-mcp-sampling-handler
           #:llm-response->mcp-create-message
           #:mcp-tool-source
           #:make-mcp-tool-source
           #:mcp-tool-source-peer))

(in-package #:ai-agent-protocol/mcp)
