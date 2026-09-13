(defpackage #:ai-agent-protocol/tests
  (:use #:cl #:rove #:ai-agent-protocol #:llm-protocol))

(in-package #:ai-agent-protocol/tests)

(defmacro with-agent-loop (&body body)
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         ,@body))))
