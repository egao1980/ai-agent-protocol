(in-package #:ai-agent-protocol)

;;; Conditions + restarts. HITL approval stays run-state (:approval / :deferred
;;; + APPROVE-INVOCATION). AGENT-APPROVAL-REQUIRED is extra: default is
;;; PAUSE-FOR-APPROVAL (signal then leave :proposed).
;;;
;;; Types whose unhandled default is "keep going" are AGENT-CONDITION, not
;;; ERROR — SIGNAL of an ERROR is visible to Rove / libuv handler-bind.

(define-condition agent-condition (condition)
  ((message :initarg :message :reader agent-error-message :initform nil)
   (run :initarg :run :reader agent-error-run :initform nil))
  (:report (lambda (c s)
             (format s "agent condition~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-error (agent-condition error) ()
  (:report (lambda (c s)
             (format s "agent error~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-missing-loop (agent-error) ()
  (:report (lambda (c s)
             (format s "bind event-protocol:*event-backend* and *event-loop*~@[: ~a~]"
                     (agent-error-message c)))))

(define-condition agent-timeout (agent-error) ()
  (:report (lambda (c s)
             (format s "agent timed out~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-canceled (agent-condition) ()
  (:report (lambda (c s)
             (format s "agent run canceled~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-unknown-tool (agent-condition)
  ((name :initarg :name :reader agent-unknown-tool-name :initform nil))
  (:report (lambda (c s)
             (format s "unknown agent tool~@[ ~s~]~@[: ~a~]"
                     (agent-unknown-tool-name c)
                     (agent-error-message c)))))

(define-condition agent-unknown-tool-error (agent-unknown-tool agent-error) ()
  (:report (lambda (c s)
             (format s "unknown agent tool~@[ ~s~]~@[: ~a~]"
                     (agent-unknown-tool-name c)
                     (agent-error-message c))))
  (:documentation "What INVOKE-TOOL-ASYNC reports to ERROR-CALLBACK for a name
its source does not offer (0.15.0): an AGENT-UNKNOWN-TOOL that is also an
ERROR, so INVOKE-TOOL's re-signal reaches HANDLER-CASE on ERROR. The loop's
own unknown-tool SIGNAL stays a plain AGENT-UNKNOWN-TOOL."))

(define-condition agent-tool-error (agent-condition)
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

(define-condition agent-generate-error (agent-condition)
  ((step :initarg :step :reader agent-generate-error-step :initform nil)
   (cause :initarg :cause :reader agent-generate-error-cause :initform nil))
  (:report (lambda (c s)
             (format s "agent generate error~@[ step ~a~]~@[: ~a~]"
                     (agent-generate-error-step c)
                     (or (agent-error-message c) (agent-generate-error-cause c))))))

(define-condition agent-max-steps (agent-condition) ()
  (:report (lambda (c s)
             (format s "agent hit max steps~@[: ~a~]" (agent-error-message c)))))

(define-condition agent-permission-save-error (agent-condition)
  ((invocation :initarg :invocation :reader agent-permission-save-error-invocation
               :initform nil)
   (rule :initarg :rule :reader agent-permission-save-error-rule :initform nil)
   (cause :initarg :cause :reader agent-permission-save-error-cause :initform nil))
  (:documentation "SIGNALed when SAVE-PERMISSION-RULE fails for an \"always\"
reply, before the store's error goes on. The invocation is already approved.
Restarts: SKIP-SAVE (INVOKE-SKIP-SAVE: keep the rule for this run only) and
APPROVE-ONCE (INVOKE-APPROVE-ONCE: drop the rules). Not an ERROR, so it
reaches handlers outside the event loop's own error handling.")
  (:report (lambda (c s)
             (format s "could not save the permission rule~@[ ~a~]~@[: ~a~]"
                     (agent-permission-save-error-rule c)
                     (agent-permission-save-error-cause c)))))

(define-condition agent-approval-required (agent-condition)
  ((invocation :initarg :invocation :reader agent-approval-required-invocation
               :initform nil))
  (:report (lambda (c s)
             (format s "agent approval required~@[ for ~s~]~@[: ~a~]"
                     (and (agent-approval-required-invocation c)
                          (agent-invocation-name
                           (agent-approval-required-invocation c)))
                     (agent-error-message c)))))

;;; --- typed tool failures --------------------------------------------------
;;; A tool handler signals one of these so the model gets a result it can act
;;; on ("fix the arguments" vs "do not call it again"), not a bare error string.

(define-condition tool-failure (error)
  ((message :initarg :message :reader tool-failure-message :initform nil))
  (:documentation "A tool failure the model can act on. Subtypes choose the
advice TOOL-FAILURE-TEXT puts in the tool result.")
  (:report (lambda (c s)
             (format s "~a" (or (tool-failure-message c) "tool failed")))))

(define-condition tool-input-error (tool-failure) ()
  (:documentation "The arguments are wrong: the model should fix them and retry."))

(define-condition tool-unavailable (tool-failure) ()
  (:documentation "The tool cannot run in this run (missing service, no access)."))

(define-condition tool-timeout (tool-failure) ()
  (:documentation "The tool ran out of time; a smaller request may work."))

(define-condition tool-permission-denied (tool-failure)
  ((action :initarg :action :reader tool-permission-denied-action :initform nil)
   (resources :initarg :resources :reader tool-permission-denied-resources
              :initform nil
              :documentation "The resources the deny came from: the ones
denied on their own, not the call's other (allowed) resources.")
   (call-resources :initarg :call-resources
                   :reader tool-permission-denied-call-resources
                   :initform nil
                   :documentation "Every resource of the call, allowed ones
included (TOOL-PERMISSION-RESOURCES).")
   (feedback :initarg :feedback :reader tool-permission-denied-feedback
             :initform nil
             :documentation "Text from the rule or the user saying what to do
instead; NIL when none was given.")
   (rule :initarg :rule :reader tool-permission-denied-rule :initform nil)
   (decision :initarg :decision :reader tool-permission-denied-decision
             :initform :deny
             :documentation "Who decided: :DENY (a rule or policy), :REJECTED
(a person turned the ask down) or :UNATTENDED (nobody could be asked, e.g. a
headless run, so the program denied it). See DENIAL-DECISION."))
  (:documentation "The call was not run: a permission rule or policy denied
it, the user rejected the approval request, or no one could be asked. Becomes
a tool result, not a run abort.")
  (:report (lambda (c s)
             (format s "~a~@[ for ~{~a~^, ~}~]~@[~%Feedback: ~a~]"
                     (case (tool-permission-denied-decision c)
                       (:rejected "the user rejected the request")
                       (:unattended "approval is not available (no one can be asked)")
                       (t "permission denied"))
                     (remove "*" (tool-permission-denied-resources c)
                             :test #'equal)
                     (tool-permission-denied-feedback c)))))

(defgeneric tool-failure-kind (condition)
  (:documentation "→ :INPUT, :UNAVAILABLE, :TIMEOUT, :DENIED or :EXECUTION for
CONDITION, the error a tool handler signalled. Specialize for a source's own
errors.")
  (:method ((condition condition)) :execution)
  (:method ((condition tool-input-error)) :input)
  (:method ((condition tool-unavailable)) :unavailable)
  (:method ((condition tool-timeout)) :timeout)
  (:method ((condition tool-permission-denied)) :denied))

(defgeneric tool-failure-text (condition name)
  (:documentation "Tool result text the model sees when tool NAME failed with
CONDITION: what went wrong plus what to do next, by TOOL-FAILURE-KIND.")
  (:method ((condition condition) name)
    (let ((msg (princ-to-string condition)))
      (ecase (tool-failure-kind condition)
        (:input
         (format nil "Tool ~a rejected its arguments: ~a~%Fix the arguments to match the tool's input schema and call it again."
                 name msg))
        (:unavailable
         (format nil "Tool ~a is unavailable: ~a~%Do not call it again in this run; continue another way."
                 name msg))
        (:timeout
         (format nil "Tool ~a timed out: ~a~%Retry with a smaller request, or continue another way."
                 name msg))
        (:denied
         (format nil "Tool ~a was not run: ~a~%Do not repeat the same call. ~:[Continue another way, or ask the user for permission.~;Follow the feedback above and continue.~]"
                 name msg (tool-permission-denied-feedback condition)))
        (:execution
         (format nil "Tool ~a failed: ~a" name msg))))))

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

(defun denial-decision (by)
  "The TOOL-PERMISSION-DENIED-DECISION for a denial made BY :USER (a person
answered: :REJECTED), :POLICY (a rule, hook or policy of the program: :DENY)
or :UNATTENDED (no one could be asked, e.g. a headless run or a timeout:
:UNATTENDED)."
  (ecase by
    (:user :rejected)
    (:policy :deny)
    (:unattended :unattended)))

(defun invoke-deny (&optional condition feedback &key (by :user))
  "Invoke DENY. With FEEDBACK the model gets a TOOL-PERMISSION-DENIED result
carrying that text (what to do instead). BY says who decided (DENIAL-DECISION):
:USER (default, \"the user rejected the request\"), :POLICY or :UNATTENDED
(a program denying because no one can be asked)."
  (let ((r (find-restart 'deny condition)))
    (when r
      (cond ((not (eq by :user)) (invoke-restart r feedback :by by))
            (feedback (invoke-restart r feedback))
            (t (invoke-restart r))))))

(defun invoke-approve-always (&optional condition)
  "Invoke APPROVE-ALWAYS: approve, and allow matching calls for the rest of the
run (see APPROVE-INVOCATION :always)."
  (let ((r (find-restart 'approve-always condition)))
    (when r (invoke-restart r))))

(defun invoke-deny-all (&optional condition feedback &key (by :user))
  "Invoke DENY-ALL: deny this and every other pending invocation of the run,
each with FEEDBACK when given (see DENY-INVOCATION :all). BY as for
INVOKE-DENY."
  (let ((r (find-restart 'deny-all condition)))
    (when r
      (cond ((not (eq by :user)) (invoke-restart r feedback :by by))
            (feedback (invoke-restart r feedback))
            (t (invoke-restart r))))))

(defun invoke-skip-save (&optional condition)
  "Invoke SKIP-SAVE: an \"always\" rule could not be saved to the permission
store; keep it for this run only."
  (let ((r (find-restart 'skip-save condition)))
    (when r (invoke-restart r))))

(defun invoke-approve-once (&optional condition)
  "Invoke APPROVE-ONCE: an \"always\" rule could not be saved; approve the
call once and drop its rules."
  (let ((r (find-restart 'approve-once condition)))
    (when r (invoke-restart r))))

(defun invoke-pause-for-approval (&optional condition)
  (let ((r (find-restart 'pause-for-approval condition)))
    (when r (invoke-restart r))))

(defun auto-skip-tool (condition)
  "HANDLER-BIND: SKIP-TOOL on tool/unknown-tool errors."
  (when (find-restart 'skip-tool condition)
    (invoke-skip-tool condition)))

(defun auto-pause-approval (condition)
  "HANDLER-BIND: PAUSE-FOR-APPROVAL (existing HITL run-state)."
  (when (find-restart 'pause-for-approval condition)
    (invoke-pause-for-approval condition)))

(defun auto-retry (condition)
  (when (find-restart 'retry condition)
    (invoke-retry condition)))

(defmacro with-auto-skip-tool (&body body)
  `(handler-bind ((agent-tool-error #'auto-skip-tool)
                  (agent-unknown-tool #'auto-skip-tool))
     ,@body))

(defmacro with-auto-pause-approval (&body body)
  `(handler-bind ((agent-approval-required #'auto-pause-approval))
     ,@body))
