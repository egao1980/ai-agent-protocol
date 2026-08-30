(in-package #:ai-agent-protocol)

(defclass agent-settings ()
  ((llm :initarg :llm :accessor agent-settings-llm :initform nil)
   (max-steps :initarg :max-steps :accessor agent-settings-max-steps :initform 20)
   (timeout :initarg :timeout :accessor agent-settings-timeout :initform nil)
   (extra :initarg :extra :accessor agent-settings-extra :initform nil)))

(defun make-agent-settings (&key llm (max-steps 20) timeout extra)
  (make-instance 'agent-settings :llm llm :max-steps max-steps
                                 :timeout timeout :extra extra))

(defun agent-settings-p (x)
  (typep x 'agent-settings))

(defun coerce-agent-settings (x)
  (cond
    ((null x) (make-agent-settings))
    ((agent-settings-p x) x)
    ((and (consp x) (keywordp (car x)))
     (make-agent-settings
      :llm (getf x :llm)
      :max-steps (or (getf x :max-steps) 20)
      :timeout (getf x :timeout)
      :extra (getf x :extra)))
    (t (error 'agent-error :message (format nil "not agent-settings: ~s" x)))))

(defclass function-tool (llm-tool)
  ((handler :initarg :handler :accessor function-tool-handler :initform nil)
   (approval-required-p :initarg :approval-required-p
                        :accessor function-tool-approval-required-p
                        :initform nil)
   (async-p :initarg :async-p :accessor function-tool-async-p :initform nil
            :documentation "When T, HANDLER is (args callback error-callback).")))

(defun make-function-tool (&key name description parameters handler
                             approval-required-p async-p)
  (check-type name string)
  (make-instance 'function-tool
                 :name name :description description :parameters parameters
                 :handler handler
                 :approval-required-p approval-required-p
                 :async-p async-p))

(defun function-tool-p (x)
  (typep x 'function-tool))

(defclass ai-agent ()
  ((name :initarg :name :accessor ai-agent-name :initform "agent")
   (backend :initarg :backend :accessor ai-agent-backend :initform nil)
   (instructions :initarg :instructions :accessor ai-agent-instructions :initform nil)
   (tools :initarg :tools :accessor ai-agent-tools :initform nil)
   (handoffs :initarg :handoffs :accessor ai-agent-handoffs :initform nil)
   (settings :initarg :settings :accessor ai-agent-settings
             :initform (make-agent-settings))))

(defun make-ai-agent (&key (name "agent") backend instructions tools handoffs settings)
  (make-instance 'ai-agent
                 :name name
                 :backend (or backend llm-protocol:*llm-backend*)
                 :instructions instructions
                 :tools (copy-list tools)
                 :handoffs (copy-list handoffs)
                 :settings (coerce-agent-settings settings)))

(defun ai-agent-p (x)
  (typep x 'ai-agent))

(defun register-agent-tool (agent tool)
  "Register TOOL (function-tool, llm-tool, or any list-agent-tools source) on AGENT."
  (check-type agent ai-agent)
  (setf (ai-agent-tools agent) (append (ai-agent-tools agent) (list tool)))
  tool)

(defun %source-name (source)
  (cond
    ((llm-tool-p source) (llm-tool-name source))
    ((ai-agent-p source) (ai-agent-name source))
    (t nil)))

(defun unregister-agent-tool (agent name)
  (check-type agent ai-agent)
  (setf (ai-agent-tools agent)
        (remove name (ai-agent-tools agent) :test #'equal :key #'%source-name))
  agent)

(defun find-agent-tool (agent name)
  (or (find-if (lambda (s)
                 (some (lambda (d) (equal name (llm-tool-name d)))
                       (list-agent-tools s)))
               (ai-agent-tools agent))
      (find name (ai-agent-handoffs agent) :key #'ai-agent-name :test #'equal)))

(defun register-agent-handoff (agent specialist)
  (check-type agent ai-agent)
  (check-type specialist ai-agent)
  (setf (ai-agent-handoffs agent)
        (append (ai-agent-handoffs agent) (list specialist)))
  specialist)

(defun unregister-agent-handoff (agent name)
  (check-type agent ai-agent)
  (setf (ai-agent-handoffs agent)
        (remove name (ai-agent-handoffs agent) :test #'equal :key #'ai-agent-name))
  agent)

(defmacro define-agent-tool (agent name (&key description parameters approval async)
                             (args)
                             &body body)
  "Register a CL function tool on AGENT.
   (define-agent-tool agent \"sum\" (:description \"add\") (args) \"3\")"
  `(register-agent-tool ,agent
     (make-function-tool :name ,name
                         :description ,description
                         :parameters ,parameters
                         :approval-required-p ,approval
                         :async-p ,async
                         :handler (lambda (,args)
                                    ,@body))))

(defclass agent-invocation ()
  ((id :initarg :id :accessor agent-invocation-id)
   (name :initarg :name :accessor agent-invocation-name)
   (arguments :initarg :arguments :accessor agent-invocation-arguments :initform "{}")
   (status :initarg :status :accessor agent-invocation-status :initform :proposed)
   (result :initarg :result :accessor agent-invocation-result :initform nil)
   (source :initarg :source :accessor agent-invocation-source :initform nil)
   (error-p :initarg :error-p :accessor agent-invocation-error-p :initform nil)
   (recorded-p :initarg :recorded-p :accessor agent-invocation-recorded-p
               :initform nil)))

(defun make-agent-invocation (&key id name (arguments "{}") (status :proposed)
                                result source error-p recorded-p)
  (make-instance 'agent-invocation
                 :id (or id (format nil "call-~a" (random (expt 36 8))))
                 :name name :arguments arguments :status status
                 :result result :source source :error-p error-p
                 :recorded-p recorded-p))

(defun agent-invocation-p (x)
  (typep x 'agent-invocation))

(defclass agent-run-handle ()
  ((canceled-p :initform nil :accessor agent-run-handle-canceled-p)
   (run :initarg :run :accessor agent-run-handle-run :initform nil)))

(defun agent-run-handle-p (x)
  (typep x 'agent-run-handle))

(defclass agent-run ()
  ((agent :initarg :agent :accessor agent-run-agent)
   (turns :initarg :turns :accessor agent-run-turns :initform nil)
   (invocations :initarg :invocations :accessor agent-run-invocations :initform nil)
   (pending :initarg :pending :accessor agent-run-pending :initform nil)
   (step :initarg :step :accessor agent-run-step :initform 0)
   (finish-reason :initarg :finish-reason :accessor agent-run-finish-reason
                  :initform nil)
   (last-response :initarg :last-response :accessor agent-run-last-response
                  :initform nil)
   (handle :initarg :handle :accessor agent-run-handle
           :initform (make-instance 'agent-run-handle))
   (on-event :initarg :on-event :accessor agent-run-on-event :initform nil)
   (on-part :initarg :on-part :accessor agent-run-on-part :initform nil)
   (in-flight-turn :initarg :in-flight-turn :accessor agent-run-in-flight-turn
                   :initform nil)
   (sources :initarg :sources :accessor agent-run-sources :initform nil)
   (extra :initarg :extra :accessor agent-run-extra :initform nil)
   (settings :initarg :settings :accessor agent-run-settings
             :initform (make-agent-settings))))

(defun make-agent-run (&key agent turns invocations pending (step 0)
                         finish-reason last-response handle on-event on-part
                         in-flight-turn sources extra settings)
  (let* ((h (or handle (make-instance 'agent-run-handle)))
         (run (make-instance 'agent-run
                             :agent agent :turns turns
                             :invocations invocations :pending pending
                             :step step :finish-reason finish-reason
                             :last-response last-response :handle h
                             :on-event on-event :on-part on-part
                             :in-flight-turn in-flight-turn
                             :sources sources :extra extra
                             :settings (coerce-agent-settings settings))))
    (setf (agent-run-handle-run h) run)
    run))

(defun agent-run-p (x)
  (typep x 'agent-run))

(defun agent-run-text (run)
  (let ((r (agent-run-last-response run)))
    (and r (llm-response-text r))))
