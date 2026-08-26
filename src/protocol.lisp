(in-package #:ai-agent-protocol)

(defgeneric list-agent-tools (source &key context)
  (:documentation "→ list of LLM-TOOL descriptors SOURCE exposes as a peer.
An AI-AGENT listed on another agent is one tool (its name), not a dump of
its internals. Owner-side flattening is COLLECT-RUN-TOOLS.")
  (:method ((source llm-tool) &key context)
    (declare (ignore context))
    (list source))
  (:method ((source list) &key context)
    (mapcan (lambda (s) (copy-list (list-agent-tools s :context context)))
            source))
  (:method ((source ai-agent) &key context)
    (declare (ignore context))
    (list (make-llm-tool :name (ai-agent-name source)
                         :description (or (ai-agent-instructions source) "")))))

(defgeneric collect-run-tools (agent &key context extra)
  (:documentation "Descriptors sent to GENERATE for a run of AGENT.
Specialize to filter/prefix/inject. EXTRA is appended to AGENT-TOOLS.")
  (:method ((agent ai-agent) &key context extra)
    (list-agent-tools (append (ai-agent-tools agent)
                              (ai-agent-handoffs agent)
                              (copy-list extra))
                      :context context)))

(defgeneric prepare-agent-turns (agent turns &key context)
  (:documentation "Normalize TURNS before the first GENERATE. :AROUND / subclass
to inject instructions or rewrite history.")
  (:method ((agent ai-agent) turns &key context)
    (declare (ignore context))
    (let ((ts (coerce-turns turns))
          (sys (ai-agent-instructions agent)))
      (if (and sys (plusp (length (string sys)))
               (not (find :system ts :key #'llm-turn-role)))
          (cons (system-turn sys) ts)
          ts))))

(defgeneric agent-approve-p (agent source name arguments &key context)
  (:documentation "T when AGENT wants HITL before SOURCE runs NAME.
Default defers to TOOL-NEEDS-APPROVAL-P on the source.")
  (:method ((agent ai-agent) source name arguments &key context)
    (tool-needs-approval-p source name arguments :context context)))

(defgeneric tool-executable-p (source name &key context)
  (:documentation "T when SOURCE can execute NAME (has a handler).")
  (:method (source name &key context)
    (declare (ignore source name context))
    nil)
  (:method ((tool function-tool) name &key context)
    (declare (ignore context))
    (and (function-tool-handler tool)
         (equal name (llm-tool-name tool))))
  (:method ((agent ai-agent) name &key context)
    (declare (ignore context))
    (equal name (ai-agent-name agent))))

(defgeneric tool-needs-approval-p (source name arguments &key context)
  (:documentation "T when invoking NAME on SOURCE should pause for approve/deny.")
  (:method (source name arguments &key context)
    (declare (ignore source name arguments context))
    nil)
  (:method ((tool function-tool) name arguments &key context)
    (declare (ignore name arguments context))
    (function-tool-approval-required-p tool)))

(defgeneric invoke-tool-async (source name arguments &key context callback
                               error-callback)
  (:documentation "Async primitive. CALLBACK gets a string (or already-stringified
result). ERROR-CALLBACK gets a condition. Returns a cancel token or NIL.
Sync handlers run off the event loop. An AI-AGENT source runs as a subagent.")
  (:method (source name arguments &key context callback error-callback)
    (declare (ignore arguments context callback))
    (let ((cb (or error-callback #'error)))
      (%call-on-loop
       (lambda ()
         (funcall cb (make-condition 'agent-unknown-tool :name name
                                     :message (format nil "no invoke for ~s on ~s"
                                                      name source)))))
      nil))
  (:method ((tool function-tool) name arguments &key context callback
            error-callback)
    (declare (ignore context))
    (let ((ok (or callback (lambda (v) (declare (ignore v)))))
          (err (or error-callback #'error))
          (fn (function-tool-handler tool)))
      (cond
        ((not (equal name (llm-tool-name tool)))
         (%call-on-loop
          (lambda ()
            (funcall err (make-condition 'agent-unknown-tool :name name))))
         nil)
        ((null fn)
         (%call-on-loop
          (lambda ()
            (funcall err (make-condition 'agent-error
                                         :message (format nil "tool ~s has no handler"
                                                          name)))))
         nil)
        ((function-tool-async-p tool)
         (funcall fn arguments ok err)
         nil)
        (t
         (%off-loop
          (lambda ()
            (let ((out (funcall fn arguments)))
              (if (stringp out) out (princ-to-string out))))
          ok err)))))
  (:method ((child ai-agent) name arguments &key context callback error-callback)
    (declare (ignore context))
    (let ((ok (or callback (lambda (v) (declare (ignore v)))))
          (err (or error-callback #'error)))
      (cond
        ((not (equal name (ai-agent-name child)))
         (%call-on-loop
          (lambda ()
            (funcall err (make-condition 'agent-unknown-tool :name name))))
         nil)
        (t
         (run-ai-agent-async
          child
          (if (and (stringp arguments) (plusp (length arguments)))
              arguments
              (or arguments ""))
          :callback (lambda (nested)
                      (funcall ok (or (agent-run-text nested) "")))
          :error-callback err))))))

(defgeneric invoke-tool (source name arguments &key context)
  (:documentation "Await INVOKE-TOOL-ASYNC. Requires a bound event loop.")
  (:method (source name arguments &key context)
    (%await (lambda (ok err)
              (invoke-tool-async source name arguments
                                 :context context
                                 :callback ok
                                 :error-callback err)))))

(defgeneric cancel-agent-run (handle)
  (:documentation "Cancel an in-flight RUN-AI-AGENT-ASYNC handle.")
  (:method ((handle agent-run-handle))
    (setf (agent-run-handle-canceled-p handle) t)
    handle)
  (:method ((run agent-run))
    (cancel-agent-run (agent-run-handle run))))

(defgeneric run-ai-agent-async (agent turns &key settings tools on-event
                                callback error-callback)
  (:documentation "Async primitive. CALLBACK gets an AGENT-RUN.
TOOLS are extra sources for this run (appended). ON-EVENT is (kind payload).
Returns AGENT-RUN-HANDLE."))

(defgeneric run-ai-agent (agent turns &key settings tools on-event)
  (:documentation "Await RUN-AI-AGENT-ASYNC (drives the bound event loop).")
  (:method ((agent ai-agent) turns &key settings tools on-event)
    (let ((timeout (agent-settings-timeout
                    (or (and settings (coerce-agent-settings settings))
                        (ai-agent-settings agent)))))
      (%await (lambda (ok err)
                (run-ai-agent-async agent turns
                                    :settings settings :tools tools
                                    :on-event on-event
                                    :callback ok :error-callback err))
              :timeout timeout))))

(defgeneric resume-ai-agent-async (run &key callback error-callback on-event)
  (:documentation "Continue a paused AGENT-RUN (after approve/deny/complete)."))

(defgeneric resume-ai-agent (run &key on-event)
  (:documentation "Await RESUME-AI-AGENT-ASYNC.")
  (:method ((run agent-run) &key on-event)
    (let ((timeout (agent-settings-timeout (agent-run-settings run))))
      (%await (lambda (ok err)
                (resume-ai-agent-async run :callback ok :error-callback err
                                       :on-event on-event))
              :timeout timeout))))

(defun approve-invocation (run invocation &key)
  (setf (agent-invocation-status invocation) :approved)
  (setf (agent-run-pending run)
        (remove invocation (agent-run-pending run)))
  run)

(defun deny-invocation (run invocation &key reason)
  (setf (agent-invocation-status invocation) :denied
        (agent-invocation-result invocation) (or reason "denied")
        (agent-invocation-error-p invocation) nil)
  (setf (agent-run-pending run)
        (remove invocation (agent-run-pending run)))
  run)

(defun complete-invocation (run invocation result &key error-p)
  "Attach an externally executed (deferred) result and mark :done."
  (setf (agent-invocation-status invocation) :done
        (agent-invocation-result invocation)
        (if (stringp result) result (princ-to-string result))
        (agent-invocation-error-p invocation) error-p)
  (setf (agent-run-pending run)
        (remove invocation (agent-run-pending run)))
  run)

(defun %parse-defagent-body (body)
  (let ((doc nil) (slots '()) (options '()))
    (when (stringp (first body))
      (setf doc (pop body)))
    (dolist (form body)
      (cond
        ((and (consp form) (keywordp (first form)))
         (push form options))
        ((and (consp form) (symbolp (first form)))
         (push form slots))
        (t (error 'agent-error :message (format nil "bad defagent form: ~s" form)))))
    (values doc (nreverse slots) (nreverse options))))

(defmacro defagent (name superclasses &body body)
  "Define an AI-AGENT subclass. Options: (:name \"x\") (:instructions \"…\")
   (:settings form) (:tools form) (:handoffs form). Slot forms like DEFCLASS.
   (defagent researcher ()
     \"Looks things up.\"
     (:name \"researcher\")
     (:instructions \"Be thorough.\"))"
  (multiple-value-bind (doc slots options)
      (%parse-defagent-body body)
    (let ((supers (or superclasses '(ai-agent)))
          (initargs '()))
      (unless (find 'ai-agent supers :test #'eq)
        (setf supers (append supers '(ai-agent))))
      (dolist (opt options)
        (ecase (first opt)
          (:name (setf initargs (list* :name (second opt) initargs)))
          (:instructions (setf initargs (list* :instructions (second opt) initargs)))
          (:settings (setf initargs (list* :settings (second opt) initargs)))
          (:tools (setf initargs (list* :tools (second opt) initargs)))
          (:handoffs (setf initargs (list* :handoffs (second opt) initargs)))))
      `(progn
         (defclass ,name ,supers
           ,slots
           ,@(when doc `((:documentation ,doc)))
           ,@(when initargs `((:default-initargs ,@initargs))))
         ',name))))
