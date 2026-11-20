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

(defun %agent-session (agent &optional context session)
  (conversation:coerce-session
   (or session
       (and (consp context) (getf context :session))
       (ai-agent-session agent)
       (ai-agent-name agent))))

(defgeneric prepare-agent-turns (agent turns &key context)
  (:documentation "Normalize TURNS before the first GENERATE. :AROUND / subclass
to inject instructions or rewrite history. Bound MEMORY recalls first.")
  (:method ((agent ai-agent) turns &key context)
    (let* ((mem (ai-agent-memory agent))
           (incoming (coerce-turns turns))
           (ts (if mem
                   (conversation:recall mem incoming
                                        :session (%agent-session agent context))
                   incoming))
           (sys (ai-agent-instructions agent))
           (ts (if (and sys (plusp (length (string sys)))
                        (not (find :system ts :key #'llm-turn-role)))
                   (cons (system-turn sys) ts)
                   ts))
           (steer (ai-agent-steering agent)))
      (if steer
          (steer:apply-steering ts steer)
          ts))))

(defgeneric agent-approve-p (agent source name arguments &key context)
  (:documentation "T when AGENT wants HITL before SOURCE runs NAME.
Default: AGENT-PERMISSION is :ASK (permission rules, else
TOOL-NEEDS-APPROVAL-P on the source).")
  (:method ((agent ai-agent) source name arguments &key context)
    (eq :ask (agent-permission agent source name arguments :context context))))

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
  (:documentation "Cancel an in-flight RUN-AI-AGENT-ASYNC handle. Background
subagents the run started are canceled too; a run waiting for them finishes
as :canceled.")
  (:method ((handle agent-run-handle))
    (setf (agent-run-handle-canceled-p handle) t)
    (let ((run (agent-run-handle-run handle)))
      (when run
        (%cancel-subagents run)))
    handle)
  (:method ((run agent-run))
    (cancel-agent-run (agent-run-handle run))))

(defun %durability-symbol (name)
  (let ((pkg (find-package '#:ai-agent-protocol/durability)))
    (and pkg (find-symbol name pkg))))

(defun %durability-fn (name)
  (let ((sym (%durability-symbol name)))
    (and sym (fboundp sym) sym)))

(defun %coerce-durability (durability)
  "Soft-load hook: :DURABILITY requires ai-agent-protocol/durability."
  (cond
    ((null durability) nil)
    (t
     (let ((fn (%durability-fn "COERCE-AGENT-DURABILITY")))
       (unless fn
         (error 'agent-error
                :message "load ai-agent-protocol/durability to use :durability"))
       (funcall fn durability)))))

(defun %durable-step-replayed (run name &key idempotency-key)
  (let ((d (and run (agent-run-durability run)))
        (fn (%durability-fn "DURABLE-STEP-REPLAYED")))
    (if (and d fn)
        (funcall fn d name :idempotency-key idempotency-key)
        (values nil nil))))

(defun %durable-record-step (run name result &key idempotency-key)
  (let ((d (and run (agent-run-durability run)))
        (fn (%durability-fn "RECORD-DURABLE-STEP")))
    (when (and d fn)
      (funcall fn d name result :idempotency-key idempotency-key))))

(defun %durable-wait-input (run prompt)
  (let ((d (and run (agent-run-durability run)))
        (fn (%durability-fn "DURABLE-WAIT-INPUT")))
    (when (and d fn)
      (funcall fn d :prompt prompt))))

(defun %durable-peek (run name key)
  "(values T result) when NAME/KEY is journaled for RUN; does not consume."
  (let ((d (and run (agent-run-durability run)))
        (fn (%durability-fn "DURABLE-STEP-RECORDED")))
    (if (and d fn)
        (funcall fn d name :idempotency-key key)
        (values nil nil))))

(defun %durable-steps (run name)
  (let ((d (and run (agent-run-durability run)))
        (fn (%durability-fn "DURABLE-RECORDED-STEPS")))
    (and d fn (funcall fn d name))))

(defun %durable-state-version (run)
  (let ((d (and run (agent-run-durability run)))
        (fn (%durability-fn "DURABLE-STATE-VERSION")))
    (and d fn (funcall fn d))))

(defun %segment-key (run base)
  "Journal key BASE in RUN's current segment. Segment 0 (the first run) keeps
the bare key, so older journals replay; each CONTINUE-AI-AGENT-ASYNC opens a
new segment, keyed \"cN/BASE\" (#16)."
  (let ((segment (if run (agent-run-segment run) 0)))
    (if (zerop segment)
        base
        (format nil "c~d/~a" segment base))))

(defun %durable-hitl-key (run invocation)
  (%segment-key run (format nil "hitl/~a" (agent-invocation-id invocation))))

(defun %journal-approval (run inv &optional rules)
  "Journal an approval of INV (with the \"always\" RULES it added) under its
hitl key. Every approval path calls this: APPROVE-INVOCATION and the APPROVE /
APPROVE-ALWAYS restarts, so replay restores the decision instead of asking (or
running the handler) again."
  (%durable-record-step run "hitl"
                        (list* :decision :approved
                               (and rules
                                    (list :rules (mapcar #'%rule-list rules))))
                        :idempotency-key (%durable-hitl-key run inv)))

(defun %journal-denial (run inv)
  "Journal the denial already set on INV (result, error-p) under its hitl key.
Called by DENY-INVOCATION and the DENY / DENY-ALL restarts."
  (%durable-record-step run "hitl"
                        (list :decision :denied
                              :reason (agent-invocation-result inv)
                              :error-p (agent-invocation-error-p inv))
                        :idempotency-key (%durable-hitl-key run inv)))

(defun %apply-journaled-hitl (run inv)
  "If approve/deny was journaled, restore it onto INV.

The decision may have come from APPROVE-INVOCATION / DENY-INVOCATION or from a
restart (APPROVE, APPROVE-ALWAYS, DENY, DENY-ALL); either way the recorded
decision is restored and AGENT-APPROVAL-REQUIRED is not signaled again.

\"Always\" rules are restored onto the run (AGENT-RUN-APPROVED-PERMISSIONS) but
not re-saved to the PERMISSION-RULE-STORE: SAVE-PERMISSION-RULE ran when the
decision was made, before the decision was journaled, so a journaled always
reply implies the store already got its rules. Re-saving would write a store
the product may have edited since (a rule the user revoked would come back)."
  (when (eq (agent-invocation-status inv) :proposed)
    (multiple-value-bind (hit result)
        (%durable-step-replayed run "hitl"
                                :idempotency-key (%durable-hitl-key run inv))
      (when hit
        (if (eq (getf result :decision) :approved)
            (progn
              ;; An "always" reply: its rules decide later calls of the run.
              (%add-approved-permissions
               run (coerce-permission-rules (getf result :rules)))
              (setf (agent-invocation-status inv) :approved))
            (setf (agent-invocation-status inv) :denied
                  (agent-invocation-result inv)
                  (or (getf result :reason) "denied")
                  (agent-invocation-error-p inv) (getf result :error-p)))
        (%emit run :invocation inv)
        t))))

(defgeneric run-ai-agent-async (agent turns &key settings tools on-event
                                on-part callback error-callback session
                                durability)
  (:documentation "Async primitive. CALLBACK gets an AGENT-RUN.
TOOLS are extra sources for this run (appended). ON-EVENT is (kind payload).
ON-PART is (lambda (llm-part)) in addition to :part on-event.
SESSION overrides AI-AGENT-SESSION for recall/remember.
DURABILITY (optional) is a journal / durable-task / agent-durability —
requires ai-agent-protocol/durability. Returns AGENT-RUN-HANDLE."))

(defgeneric run-ai-agent (agent turns &key settings tools on-event on-part
                          session durability)
  (:documentation "Await RUN-AI-AGENT-ASYNC (drives the bound event loop).")
  (:method ((agent ai-agent) turns &key settings tools on-event on-part session
            durability)
    (let ((timeout (agent-settings-timeout
                    (or (and settings (coerce-agent-settings settings))
                        (ai-agent-settings agent)))))
      (%await (lambda (ok err)
                (run-ai-agent-async agent turns
                                    :settings settings :tools tools
                                    :on-event on-event :on-part on-part
                                    :session session
                                    :durability durability
                                    :callback ok :error-callback err))
              :timeout timeout))))

(defgeneric resume-ai-agent-async (run &key callback error-callback on-event
                                  on-part durability)
  (:documentation "Continue a paused AGENT-RUN (after approve/deny/complete)."))

(defgeneric resume-ai-agent (run &key on-event on-part durability)
  (:documentation "Await RESUME-AI-AGENT-ASYNC.")
  (:method ((run agent-run) &key on-event on-part durability)
    (let ((timeout (agent-settings-timeout (agent-run-settings run))))
      (%await (lambda (ok err)
                (resume-ai-agent-async run :callback ok :error-callback err
                                       :on-event on-event :on-part on-part
                                       :durability durability))
              :timeout timeout))))

(defun approve-invocation (run invocation &key always)
  "Approve a :proposed INVOCATION once. With ALWAYS (opencode's \"always\"
reply) also allow matching calls for the rest of RUN: one :allow rule per
TOOL-PERMISSION-ALWAYS-RESOURCES goes on AGENT-RUN-APPROVED-PERMISSIONS and
through SAVE-PERMISSION-RULE into the run's PERMISSION-RULE-STORE, and every
other pending invocation those rules now allow is approved too. The rules only
answer asks: a deny rule or a policy still wins.

INVOCATION is approved before the rules are saved: when SAVE-PERMISSION-RULE
signals, it stays approved (the rules still hold for RUN), and the restarts
SKIP-SAVE (per rule) and APPROVE-ONCE (drop the rules) are available."
  (let ((rules (and always (%always-rules run invocation))))
    (setf (agent-invocation-status invocation) :approved)
    (setf (agent-run-pending run)
          (remove invocation (agent-run-pending run)))
    (unwind-protect
         (when rules
           (setf rules (%remember-approval run invocation rules)))
      (%journal-approval run invocation rules)
      (%emit run :invocation invocation))
    (when rules
      (dolist (other (%proposed-invocations run))
        (when (%now-allowed-p run other)
          (approve-invocation run other)))))
  run)

(defun deny-invocation (run invocation &key reason feedback all (by :user))
  "Reject a :proposed INVOCATION. With FEEDBACK the model gets a
TOOL-PERMISSION-DENIED error result carrying it (what to do instead);
otherwise the result is REASON (default \"denied\"). BY says who decided
(DENIAL-DECISION): :USER (default), :POLICY or :UNATTENDED; any but :USER
always gives a TOOL-PERMISSION-DENIED result. With ALL (opencode's
\"a rejection rejects every pending request\") every other :proposed
invocation of RUN is rejected the same way."
  (if (or feedback (not (eq by :user)))
      (%refuse-invocation nil invocation (%rejection invocation feedback by))
      (setf (agent-invocation-status invocation) :denied
            (agent-invocation-result invocation) (or reason "denied")
            (agent-invocation-error-p invocation) nil))
  (setf (agent-run-pending run)
        (remove invocation (agent-run-pending run)))
  (%journal-denial run invocation)
  (%emit run :invocation invocation)
  (when all
    (dolist (other (%proposed-invocations run))
      (deny-invocation run other :reason reason :feedback feedback :by by)))
  run)

(defun complete-invocation (run invocation result &key error-p)
  "Attach an externally executed (deferred) result and mark :done."
  (setf (agent-invocation-status invocation) :done
        (agent-invocation-result invocation)
        (if (stringp result) result (princ-to-string result))
        (agent-invocation-error-p invocation) error-p)
  (setf (agent-run-pending run)
        (remove invocation (agent-run-pending run)))
  (%emit run :invocation invocation)
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
   (:settings form) (:tools form) (:handoffs form) (:memory form) (:session form)
   (:steering form) (:permissions form) (:permission-store form) (:model form).
   Slot forms like DEFCLASS.
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
          (:handoffs (setf initargs (list* :handoffs (second opt) initargs)))
          (:memory (setf initargs (list* :memory (second opt) initargs)))
          (:session (setf initargs (list* :session (second opt) initargs)))
          (:steering (setf initargs (list* :steering (second opt) initargs)))
          (:permissions (setf initargs (list* :permissions (second opt) initargs)))
          (:permission-store
           (setf initargs (list* :permission-store (second opt) initargs)))
          (:model (setf initargs (list* :model (second opt) initargs)))))
      `(progn
         (defclass ,name ,supers
           ,slots
           ,@(when doc `((:documentation ,doc)))
           ,@(when initargs `((:default-initargs ,@initargs))))
         ',name))))
