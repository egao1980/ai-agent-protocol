(in-package #:ai-agent-protocol)

;;; Conditions + restarts. HITL approval stays run-state (:approval / :deferred
;;; + APPROVE-INVOCATION). AGENT-APPROVAL-REQUIRED is extra: default is
;;; PAUSE-FOR-APPROVAL (signal then leave :proposed).

(define-condition agent-error (error)
  ((message :initarg :message :reader agent-error-message :initform nil)
   (run :initarg :run :reader agent-error-run :initform nil))
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

(define-condition agent-tool-error (agent-error)
  ((invocation :initarg :invocation :reader agent-tool-error-invocation :initform nil)
   (cause :initarg :cause :reader agent-tool-error-cause :initform nil)
   (name :initarg :name :reader agent-tool-error-name :initform nil))
  (:report (lambda (c s)
             (format s "agent tool error~@[ ~s~]~@[: ~a~]"
                     (or (agent-tool-error-name c)
                         (and (agent-tool-error-invocation c)
                              (agent-invocation-name (agent-tool-error-invocation c))))
                     (or (agent-error-message c)
                         (agent-tool-error-cause c))))))

(define-condition agent-generate-error (agent-error)
  ((step :initarg :step :reader agent-generate-error-step :initform nil)
   (cause :initarg :cause :reader agent-generate-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "agent generate error~@[ step ~a~]~@[: ~a~]"
                     (agent-generate-error-step c)
                     (or (agent-error-message c) (agent-generate-error-cause c))))))

(define-condition agent-max-steps (agent-error) ()
  (:report (lambda (c s)
             (format s "agent hit max steps~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-approval-required (agent-error)
  ((invocation :initarg :invocation :reader agent-approval-required-invocation
               :initform nil))
  (:report (lambda (c s)
             (format s "agent approval required~@[ for ~s~]~@[: ~a~]"
                     (and (agent-approval-required-invocation c)
                          (agent-invocation-name
                           (agent-approval-required-invocation c)))
                     (agent-error-message c)))))

;;; --- restart helpers -------------------------------------------------------

(defun call-with-agent-restarts (thunk)
  "Establish RETRY / USE-VALUE around THUNK."
  (tagbody
   :retry
     (return-from call-with-agent-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry the agent operation"
           (go :retry))
         (use-value (value)
           :report "Use a supplied value instead"
           :interactive (lambda ()
                          (format *query-io* "Value to use: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           value)))))

(defmacro with-agent-restarts (&body body)
  `(call-with-agent-restarts (lambda () ,@body)))

(defun invoke-retry (&optional condition)
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun invoke-use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun invoke-skip-tool (&optional condition)
  (let ((r (find-restart 'skip-tool condition)))
    (when r (invoke-restart r))))

(defun invoke-approve (&optional condition)
  (let ((r (find-restart 'approve condition)))
    (when r (invoke-restart r))))

(defun invoke-deny (&optional condition)
  (let ((r (find-restart 'deny condition)))
    (when r (invoke-restart r))))

(defun invoke-pause-for-approval (&optional condition)
  (let ((r (find-restart 'pause-for-approval condition)))
    (when r (invoke-restart r))))

(defun invoke-continue-run (&optional condition &rest args)
  (let ((r (find-restart 'continue-run condition)))
    (when r (apply #'invoke-restart r args))))

(defun invoke-abort-run (&optional condition)
  (let ((r (find-restart 'abort-run condition)))
    (when r (invoke-restart r))))

(defun auto-skip-tool (condition)
  "HANDLER-BIND: SKIP-TOOL on tool/unknown-tool errors."
  (if (find-restart 'skip-tool condition)
      (invoke-skip-tool condition)
      (error condition)))

(defun auto-pause-approval (condition)
  "HANDLER-BIND: PAUSE-FOR-APPROVAL (existing HITL run-state)."
  (if (find-restart 'pause-for-approval condition)
      (invoke-pause-for-approval condition)
      (error condition)))

(defun auto-retry (condition)
  (if (find-restart 'retry condition)
      (invoke-retry condition)
      (error condition)))

(defmacro with-auto-skip-tool (&body body)
  `(handler-bind ((agent-tool-error #'auto-skip-tool)
                  (agent-unknown-tool #'auto-skip-tool))
     ,@body))

(defmacro with-auto-pause-approval (&body body)
  `(handler-bind ((agent-approval-required #'auto-pause-approval))
     ,@body))
