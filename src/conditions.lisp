(in-package #:ai-agent-protocol)

(define-condition agent-error (error)
  ((message :initarg :message :reader agent-error-message :initform nil))
  (:report (lambda (c s)
             (format s "agent error~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-missing-loop (agent-error) ()
  (:report (lambda (c s)
             (format s "bind event-protocol:*event-backend* and *event-loop*~@[: ~a~]"
                     (agent-error-message c)))))

(define-condition agent-timeout (agent-error) ()
  (:report (lambda (c s)
             (format s "agent timed out~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-canceled (agent-error) ()
  (:report (lambda (c s)
             (format s "agent run canceled~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-unknown-tool (agent-error)
  ((name :initarg :name :reader agent-unknown-tool-name :initform nil))
  (:report (lambda (c s)
             (format s "unknown agent tool~@[ ~s~]~@[: ~a~]"
                     (agent-unknown-tool-name c)
                     (agent-error-message c)))))
