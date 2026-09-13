(defpackage #:ai-agent-protocol/telemetry
  (:use #:cl #:ai-agent-protocol #:llm-protocol #:telemetry-protocol)
  (:documentation "Optional GenAI spans around RUN-AI-AGENT / INVOKE-TOOL-ASYNC."))

(in-package #:ai-agent-protocol/telemetry)
