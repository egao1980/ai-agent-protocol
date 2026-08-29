(defpackage #:ai-agent-protocol/ag-ui
  (:use #:cl #:ai-agent-protocol)
  (:export #:ag-ui-encoder
           #:make-ag-ui-encoder
           #:encode-agent-event
           #:ag-ui-encoder-events
           #:start-ag-ui-agent-run
           #:make-ai-agent-ag-ui-handler
           #:ag-ui-message->turn
           #:ag-ui-input-turns
           ;; interrupts / human-in-the-loop
           #:invocation-interrupt
           #:run-interrupts
           #:mark-tool-resumed
           #:apply-resume
           #:resume-ag-ui-agent-run))

(in-package #:ai-agent-protocol/ag-ui)
