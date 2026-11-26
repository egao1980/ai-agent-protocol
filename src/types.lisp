(in-package #:ai-agent-protocol)

(defclass agent-settings ()
  ((llm :initarg :llm :accessor agent-settings-llm :initform nil)
   (max-steps :initarg :max-steps :accessor agent-settings-max-steps :initform 20)
   (timeout :initarg :timeout :accessor agent-settings-timeout :initform nil)
   (extra :initarg :extra :accessor agent-settings-extra :initform nil)
   (model :initarg :model :accessor agent-settings-model :initform nil
          :documentation "Model id for this run, passed as :model to GENERATE /
STREAM-GENERATE; overrides AI-AGENT-MODEL. NIL → the agent's model.")))

(defun make-agent-settings (&key llm (max-steps 20) timeout extra model)
  (make-instance 'agent-settings :llm llm :max-steps max-steps
                                 :timeout timeout :extra extra :model model))

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
      :extra (getf x :extra)
      :model (getf x :model)))
    (t (error 'agent-error :message (format nil "not agent-settings: ~s" x)))))

(defclass function-tool (llm-tool)
  ((handler :initarg :handler :accessor function-tool-handler :initform nil)
   (approval-required-p :initarg :approval-required-p
                        :accessor function-tool-approval-required-p
                        :initform nil)
   (async-p :initarg :async-p :accessor function-tool-async-p :initform nil
            :documentation "When T, HANDLER is (args callback error-callback).")
   (permission-resources :initarg :permission-resources
                         :accessor function-tool-permission-resources
                         :initform nil
                         :documentation "NIL or (lambda (arguments)) → list of
resource strings (paths, commands) the permission rules match against.")))

(defun make-function-tool (&key name description parameters handler
                             approval-required-p async-p permission-resources)
  (check-type name string)
  (make-instance 'function-tool
                 :name name :description description :parameters parameters
                 :handler handler
                 :approval-required-p approval-required-p
                 :async-p async-p
                 :permission-resources permission-resources))

(defun function-tool-p (x)
  (typep x 'function-tool))

(defclass ai-agent ()
  ((name :initarg :name :accessor ai-agent-name :initform "agent")
   (backend :initarg :backend :accessor ai-agent-backend :initform nil)
   (model :initarg :model :accessor ai-agent-model :initform nil
          :documentation "Model id passed as :model to the backend's GENERATE /
STREAM-GENERATE, or NIL for the backend's default model. Run settings :model
overrides it.")
   (instructions :initarg :instructions :accessor ai-agent-instructions :initform nil)
   (tools :initarg :tools :accessor ai-agent-tools :initform nil)
   (handoffs :initarg :handoffs :accessor ai-agent-handoffs :initform nil)
   (settings :initarg :settings :accessor ai-agent-settings
             :initform (make-agent-settings))
   (memory :initarg :memory :accessor ai-agent-memory :initform nil)
   (session :initarg :session :accessor ai-agent-session :initform nil)
   (steering :initarg :steering :accessor ai-agent-steering :initform nil)
   (permissions :initarg :permissions :accessor ai-agent-permissions
                :initform nil
                :documentation "Ordered PERMISSION-RULEs (agent layer). Run
settings extra :permissions are appended after them; the last match wins.")
   (permission-store :initarg :permission-store :accessor ai-agent-permission-store
                     :initform nil
                     :documentation "PERMISSION-RULE-STORE where \"always\"
approvals are saved and read back, or NIL. Run settings extra
:permission-store overrides it.")))

(defun make-ai-agent (&key (name "agent") backend model instructions tools handoffs settings
                        memory session steering permissions permission-store)
  (make-instance 'ai-agent
                 :name name
                 :backend (or backend llm-protocol:*llm-backend*)
                 :model model
                 :instructions instructions
                 :tools (copy-list tools)
                 :handoffs (copy-list handoffs)
                 :settings (coerce-agent-settings settings)
                 :memory memory
                 :session session
                 :steering (and steering (steer:coerce-steering steering))
                 :permissions permissions
                 :permission-store permission-store))

(defun ai-agent-p (x)
  (typep x 'ai-agent))

(defmethod initialize-instance :after ((agent ai-agent) &key)
  (setf (ai-agent-permissions agent)
        (coerce-permission-rules (ai-agent-permissions agent)))
  (when (ai-agent-steering agent)
    (setf (ai-agent-steering agent)
          (steer:coerce-steering (ai-agent-steering agent)))))

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
               :initform nil)
   ;; 0.15.0 (#37, given): an AGENT-TOOL-OUTPUT's parts and structured content
   (parts :initarg :parts :accessor agent-invocation-parts :initform nil)
   (structured :initarg :structured :accessor agent-invocation-structured :initform nil)
   ;; 0.16.1 (#51): the output asked to end the run (cleared once it has)
   (end-run-p :initarg :end-run-p :accessor agent-invocation-end-run-p :initform nil)))

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
   (segment :accessor agent-run-segment :initform 0
            :documentation "0 for the first run, +1 per CONTINUE-AI-AGENT-ASYNC:
keeps each continuation's journal keys apart (steps restart at 1).")
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
             :initform (make-agent-settings))
   (session :initarg :session :accessor agent-run-session :initform nil)
   (memory :initarg :memory :accessor agent-run-memory :initform nil)
   (durability :initarg :durability :accessor agent-run-durability :initform nil)
   (usage-anchor :accessor agent-run-usage-anchor :initform nil
                 :documentation "Turn count the last response's usage measured
(prompt + reply); turns after it are estimated. NIL after a compaction.")
   (generate-attempt :accessor agent-run-generate-attempt :initform 1
                     :documentation "1-based attempt of the current generate step.")
   (compacted-step :accessor agent-run-compacted-step :initform nil
                   :documentation "Step that already compacted on overflow.")
   (wrap-up-p :accessor agent-run-wrap-up-p :initform nil
              :documentation "T during the tool-less generate after max steps.")
   (parent :initarg :parent :accessor agent-run-parent :initform nil
           :documentation "The AGENT-RUN that started this one as a background
subagent, or NIL.")
   (inbox :accessor agent-run-inbox :initform nil
          :documentation "Turns queued for this run (subagent reports, follow-up
messages), oldest first; injected before the next generate.")
   (inbox-lock :reader agent-run-inbox-lock
               :initform (bt:make-lock "ai-agent-inbox"))
   (subagents :accessor agent-run-subagents :initform nil
              :documentation "SUBAGENT-TASKs this run started, oldest first.")
   (waiter :accessor agent-run-waiter :initform nil
           :documentation "Thunk that resumes a run waiting for its subagents,
or NIL. Called on the loop when a report arrives or the run is canceled.")
   (instructions :accessor agent-run-instructions :initform nil
                 :documentation "This run's STEER:INSTRUCTION-DISCOVERY (a copy of
the configured one, so what is in context is per run), or NIL.")
   (instructions-scanned :accessor agent-run-instructions-scanned :initform nil
                         :documentation "Invocations already scanned for paths.")
   (instructions-reinject :accessor agent-run-instructions-reinject :initform nil
                          :documentation "Instruction files to re-inject after a
compaction dropped them from context.")
   (approved-permissions :accessor agent-run-approved-permissions :initform nil
                         :documentation ":allow PERMISSION-RULEs from \"always\"
approvals in this run, oldest first. They only answer asks.")
   (journal-state :accessor agent-run-journal-state :initform nil
                  :documentation "RUN-JOURNAL-STATE when durability journals run
state (subagents, inbox, instructions), else NIL.")
   ;; 0.15.0 (#37): internal, not journaled
   (rich-tool-turns :accessor %agent-run-rich-tool-turns :initform nil
                    :documentation "Alist (TOOL-TURN . INVOCATION) of tool turns
whose invocation carries AGENT-INVOCATION-PARTS, matched by EQ when the turns
go to GENERATE (src/loop.lisp %GENERATE-TURNS).")
   ;; 0.16.0 (#39): internal, not journaled (src/resilience.lisp)
   (continuations :accessor %agent-run-continuations :initform 0
                  :documentation "Continuations since the last response that did
not end :LENGTH (rule 3).")
   (continuing-p :accessor %agent-run-continuing-p :initform nil
                 :documentation "T while the next response continues a chain.")
   (chain-text :accessor %agent-run-chain-text :initform nil
               :documentation "Concatenated text of the continuation chain that
ends with CHAIN-RESPONSE, or NIL.")
   (chain-response :accessor %agent-run-chain-response :initform nil
                   :documentation "The response CHAIN-TEXT ends with.")
   (length-retry-step :accessor %agent-run-length-retry-step :initform nil
                      :documentation "Step regenerated with raised max tokens for
a truncated tool call (rule 4a), or NIL.")
   (manual-compactions :accessor %agent-run-manual-compactions :initform 0
                       :documentation "COMPACT-AGENT-RUN calls (journal keys).")))

(defstruct (run-journal-state (:conc-name run-journal-))
  "Inbox bookkeeping of a run whose journal carries run state."
  (replay nil :type list)    ; journaled posts not yet drained (results), oldest first
  (posted 0 :type fixnum)    ; posts journaled so far
  (drained 0 :type fixnum))  ; posts delivered so far

(defvar *subagent-parent* nil
  "Bound to the parent AGENT-RUN while it starts a background subagent;
RUN-AI-AGENT-ASYNC records it as the new run's AGENT-RUN-PARENT.")

(defun make-agent-run (&key agent turns invocations pending (step 0)
                         finish-reason last-response handle on-event on-part
                         in-flight-turn sources extra settings session memory
                         durability)
  (let* ((h (or handle (make-instance 'agent-run-handle)))
         (run (make-instance 'agent-run
                             :agent agent :turns turns
                             :invocations invocations :pending pending
                             :step step :finish-reason finish-reason
                             :last-response last-response :handle h
                             :on-event on-event :on-part on-part
                             :in-flight-turn in-flight-turn
                             :sources sources :extra extra
                             :settings (coerce-agent-settings settings)
                             :session session
                             :memory memory
                             :durability durability)))
    (setf (agent-run-handle-run h) run)
    run))

(defun agent-run-p (x)
  (typep x 'agent-run))

(defun agent-run-text (run)
  "Text of the run's last response; of a continued reply (0.16.0 rule 3), the
concatenated texts of the chain, first to last."
  (let ((r (agent-run-last-response run)))
    (cond ((null r) nil)
          ((and (%agent-run-chain-text run) (eq r (%agent-run-chain-response run)))
           (%agent-run-chain-text run))
          (t (llm-response-text r)))))
