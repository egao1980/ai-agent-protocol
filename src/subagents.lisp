(in-package #:ai-agent-protocol)

;;; Background subagents. SUBAGENT-TOOL lets the model start a nested agent
;;; that runs beside the parent on the same event loop; the call returns an
;;; id at once. When the child goes idle its last reply is queued in the
;;; parent's inbox and injected before the parent's next generate as a user
;;; turn <subagent id=… name=…>text</subagent>. Calling the tool again with
;;; the id sends a follow-up (queued if the child is busy, else it continues).
;;; One level only: a child run cannot start subagents. A parent that would
;;; finish with children still running waits for them (settings extra
;;; :subagents-on-finish :wait, or :cancel / :detach). Canceling the parent
;;; cancels its children.

(defclass subagent-task ()
  ((id :initarg :id :reader subagent-task-id)
   (agent :initarg :agent :reader subagent-task-agent)
   (parent :initarg :parent :reader subagent-task-parent)
   (run :initarg :run :accessor subagent-task-run :initform nil
        :documentation "The child AGENT-RUN.")
   (status :initarg :status :accessor subagent-task-status :initform :running
           :documentation ":RUNNING, :IDLE (finished, can be continued),
:PAUSED (waiting for approval), :FAILED or :CANCELED.")
   (result :accessor subagent-task-result :initform nil
           :documentation "Last reply text (or failure text) reported.")
   (restored-p :accessor subagent-task-restored-p :initform nil
               :documentation "T for a task rebuilt from the journal: no child
runs for it in this process."))
  (:documentation "One background subagent started by a parent run."))

(defun subagent-task-name (task)
  (ai-agent-name (subagent-task-agent task)))

(defmethod print-object ((task subagent-task) stream)
  (print-unreadable-object (task stream :type t)
    (format stream "~a ~a ~s" (subagent-task-id task) (subagent-task-name task)
            (subagent-task-status task))))

(defun find-subagent-task (run id)
  "The SUBAGENT-TASK of RUN with ID, or NIL."
  (find id (agent-run-subagents run) :key #'subagent-task-id :test #'equal))

(defun %live-subagents (run)
  (remove-if-not (lambda (task) (eq (subagent-task-status task) :running))
                 (agent-run-subagents run)))

;;; --- inbox ------------------------------------------------------------------

(defun agent-run-post (run turn)
  "Queue TURN (an LLM-TURN or a string, sent as a user turn) for RUN. It is
injected before RUN's next generate; a run waiting for its subagents wakes."
  (%post run turn))

(defun %post (run turn &key task)
  "AGENT-RUN-POST; TASK is the SUBAGENT-TASK whose report TURN is (its state
is journaled with the post)."
  (let ((turn (if (stringp turn) (user-turn turn) turn)))
    (bt:with-lock-held ((agent-run-inbox-lock run))
      (%journal-post run turn task)
      (setf (agent-run-inbox run) (append (agent-run-inbox run) (list turn))))
    (let ((waiter (agent-run-waiter run)))
      (when waiter
        (setf (agent-run-waiter run) nil)
        (funcall waiter)))
    run))

(defun %drain-inbox (run)
  "Append queued inbox turns to RUN's history (before a generate)."
  (let ((turns (if (agent-run-journal-state run)
                   (%journaled-drain run)
                   (bt:with-lock-held ((agent-run-inbox-lock run))
                     (shiftf (agent-run-inbox run) nil)))))
    (when turns
      (setf (agent-run-turns run) (append (agent-run-turns run) turns))
      (%emit run :inbox turns))
    turns))

(defun %subagent-report-turn (task text &optional status)
  (user-turn (format nil "<subagent id=\"~a\" name=\"~a\"~@[ status=\"~(~a~)\"~]>~a</subagent>"
                     (subagent-task-id task) (subagent-task-name task)
                     status (or text ""))))

;;; --- finishing and waiting --------------------------------------------------

(defun %finish-or-await-subagents (run reason callback error-callback)
  "The model ended its turn. Inbox turns pending → one more step. Live
subagents → per settings extra :subagents-on-finish: :wait (default) parks the
run until a report arrives, :cancel cancels them, :detach leaves them."
  (let* ((policy (%extra run :subagents-on-finish :wait))
         (state (agent-run-journal-state run))
         (key (format nil "inbox/continue/~a" (%generate-step-key run))))
    (flet ((continue-run ()
             (when state
               (%durable-record-step run "inbox" t :idempotency-key key))
             (%tick-run run callback error-callback)))
      (when state
        (when (%durable-peek run "inbox" key)
          ;; The journaled run went on from here: replay that.
          (return-from %finish-or-await-subagents (%tick-run run callback error-callback)))
        ;; Past the journal: undelivered posts are inbox items again, and
        ;; children that were running at the crash will never report.
        (%flush-replay-posts run)
        (when (eq policy :wait)
          (%fail-lost-subagents run)))
      (let ((live (%live-subagents run)))
        (cond
          ((agent-run-inbox run)
           (continue-run))
          ((and live (eq policy :wait))
           (multiple-value-bind (eb el) (%event-context)
             (setf (agent-run-waiter run)
                   (lambda ()
                     (event:wake-call eb el #'continue-run))))
           (%emit run :waiting live))
          (t
           (when (and live (eq policy :cancel))
             (%cancel-subagents run))
           (when state
             ;; :detach — lost children's failure reports stay queued, like
             ;; any report that lands after the parent finished.
             (%fail-lost-subagents run))
           (%finish run reason callback)))))))

(defun %cancel-subagents (run)
  "Cancel RUN's live subagents. A run parked waiting for them wakes and
finishes as :canceled when its own handle is canceled."
  (dolist (task (agent-run-subagents run))
    (when (member (subagent-task-status task) '(:running :paused) :test #'eq)
      (setf (subagent-task-status task) :canceled)
      (let ((child (subagent-task-run task)))
        (when child
          (cancel-agent-run (agent-run-handle child))))))
  (let ((waiter (agent-run-waiter run)))
    (when (and waiter (%canceled-p run))
      (setf (agent-run-waiter run) nil)
      (funcall waiter)))
  run)

(defun %subagent-done (task child)
  (let ((parent (subagent-task-parent task))
        (reason (agent-run-finish-reason child)))
    (unless (eq (subagent-task-status task) :canceled)
      (cond
        ((eq reason :canceled)
         (setf (subagent-task-status task) :canceled))
        (t
         (let ((paused (member reason '(:approval :deferred) :test #'eq)))
           (setf (subagent-task-status task) (if paused :paused :idle)
                 (subagent-task-result task)
                 (if paused
                     (format nil "Paused waiting for approval of ~{~a~^, ~}."
                             (mapcar #'agent-invocation-name (agent-run-pending child)))
                     (or (agent-run-text child) "")))
           (%emit parent :subagent (list :event :reported :task task))
           (%post parent (%subagent-report-turn
                          task (subagent-task-result task)
                          (and paused :paused))
                  :task task)))))))

(defun %subagent-failed (task condition)
  (let ((parent (subagent-task-parent task)))
    (unless (eq (subagent-task-status task) :canceled)
      (setf (subagent-task-status task) :failed
            (subagent-task-result task) (princ-to-string condition))
      (%emit parent :subagent (list :event :failed :task task :condition condition))
      (%post parent (%subagent-report-turn
                     task (subagent-task-result task) :failed)
             :task task))))

;;; --- starting and continuing --------------------------------------------------

(defun %next-subagent-id (run)
  (format nil "sub-~d" (1+ (length (agent-run-subagents run)))))

(defun start-subagent (parent agent prompt)
  "Start AGENT on PROMPT in the background of PARENT (an AGENT-RUN) and
return its SUBAGENT-TASK. The child's report lands in PARENT's inbox."
  (check-type agent ai-agent)
  (when (agent-run-parent parent)
    (error 'tool-unavailable
           :message "a background subagent cannot start subagents of its own; do the work yourself"))
  (let ((task (make-instance 'subagent-task :id (%next-subagent-id parent)
                                            :agent agent :parent parent)))
    (setf (agent-run-subagents parent)
          (append (agent-run-subagents parent) (list task)))
    (let* ((handle (let ((*subagent-parent* parent))
                     (run-ai-agent-async
                      agent prompt
                      :callback (lambda (child) (%subagent-done task child))
                      :error-callback (lambda (c) (%subagent-failed task c))))))
      (setf (subagent-task-run task) (agent-run-handle-run handle)))
    (%emit parent :subagent (list :event :started :task task))
    task))

(defun continue-ai-agent-async (run message &key callback error-callback)
  "Continue a finished (non-paused) RUN with a follow-up user MESSAGE (string
or LLM-TURN): the step count restarts and the loop runs to the next finish.
Each continuation opens a new journal segment (AGENT-RUN-SEGMENT), so its
steps never replay the earlier segments' records."
  (when (member (agent-run-finish-reason run) '(:approval :deferred) :test #'eq)
    (error 'agent-error :run run
                        :message "run is paused for approval; resume it instead"))
  (%event-context)
  (setf (agent-run-finish-reason run) nil
        (agent-run-step run) 0
        (agent-run-segment run) (1+ (agent-run-segment run))
        (agent-run-wrap-up-p run) nil
        (agent-run-compacted-step run) nil
        (agent-run-generate-attempt run) 1
        (agent-run-turns run)
        (append (agent-run-turns run)
                (list (if (stringp message) (user-turn message) message))))
  (let ((ok (or callback (lambda (v) (declare (ignore v)))))
        (err (or error-callback #'error)))
    (%call-on-loop (lambda () (%tick-run run ok err)))
    (agent-run-handle run)))

(defun continue-ai-agent (run message)
  "Await CONTINUE-AI-AGENT-ASYNC (drives the bound event loop). → RUN."
  (%await (lambda (ok err)
            (continue-ai-agent-async run message :callback ok :error-callback err))
          :timeout (agent-settings-timeout (agent-run-settings run))))

(defun continue-subagent (task message)
  "Send MESSAGE to TASK: queued in a running child's inbox, or continues an
idle child. → :QUEUED or :CONTINUED."
  (ecase (subagent-task-status task)
    (:running
     (agent-run-post (subagent-task-run task) message)
     :queued)
    (:idle
     (unless (subagent-task-run task)
       (error 'tool-unavailable
              :message (format nil "subagent ~a's history was not restored; start a new one"
                               (subagent-task-id task))))
     (setf (subagent-task-status task) :running
           (subagent-task-restored-p task) nil)
     (continue-ai-agent-async
      (subagent-task-run task) message
      :callback (lambda (child) (%subagent-done task child))
      :error-callback (lambda (c) (%subagent-failed task c)))
     :continued)
    (:paused
     (error 'tool-unavailable
            :message (format nil "subagent ~a is waiting for approval and cannot take a message"
                             (subagent-task-id task))))
    ((:failed :canceled)
     (error 'tool-unavailable
            :message (format nil "subagent ~a has ~(~a~); start a new one"
                             (subagent-task-id task) (subagent-task-status task))))))

;;; --- the tool -----------------------------------------------------------------

(defclass subagent-tool (llm-tool)
  ((agents :initarg :agents :accessor subagent-tool-agents :initform nil
           :documentation "AI-AGENTs the model may start, by name."))
  (:documentation "Tool that starts or continues background subagents."))

(defun subagent-tool-p (x)
  (typep x 'subagent-tool))

(defun %json-object (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defparameter +subagent-tool-description+
  "Start a subagent in the background. It returns an id at once and the subagent works while you continue. Do not poll and do not wait for it: when it finishes, its result arrives in your inbox as a <subagent id=\"...\" name=\"...\"> message. To send a follow-up to a subagent, call this tool again with its id and a prompt."
  "Lead of the SUBAGENT-TOOL description; the agents it may start follow.")

(defun make-subagent-tool (agents &key (name "subagent") description)
  "Tool that lets the model start AGENTS (a list of AI-AGENTs) in the
background. Arguments: {\"agent\": name, \"prompt\": text} to start one,
{\"id\": id, \"prompt\": text} to continue one."
  (let ((agents (copy-list agents)))
    (dolist (a agents) (check-type a ai-agent))
    (make-instance
     'subagent-tool
     :name name
     :agents agents
     :description
     (or description
         (format nil "~a~%~%Agents:~%~:{- ~a: ~a~%~}" +subagent-tool-description+
                 (mapcar (lambda (a)
                           (list (ai-agent-name a)
                                 (or (ai-agent-instructions a) "")))
                         agents)))
     :parameters
     (%json-object
      "type" "object"
      "properties"
      (%json-object
       "agent" (%json-object "type" "string"
                             "enum" (coerce (mapcar #'ai-agent-name agents) 'vector)
                             "description" "Agent to start (omit when continuing by id).")
       "prompt" (%json-object "type" "string"
                              "description" "Task for a new subagent, or the follow-up message.")
       "id" (%json-object "type" "string"
                          "description" "Id of an earlier subagent to continue."))
      "required" (vector "prompt")))))

(defun %arg (args key)
  (let ((v (gethash key args)))
    (and (stringp v) (plusp (length v)) v)))

(defun %subagent-arguments (arguments)
  "→ hash-table of the tool ARGUMENTS (JSON text or a table). Signals
TOOL-INPUT-ERROR on anything else; TOOL-UNAVAILABLE without a JSON backend."
  (cond
    ((hash-table-p arguments) arguments)
    ((not (stringp arguments))
     (error 'tool-input-error :message "arguments must be a JSON object"))
    ((null (and (find-package '#:json-protocol)
                (let ((sym (find-symbol "*JSON-BACKEND*" '#:json-protocol)))
                  (and sym (symbol-value sym)))))
     (error 'tool-unavailable
            :message "no json-protocol backend is loaded to read the arguments"))
    (t
     (let ((parsed (try-parse-json-output arguments)))
       (unless (hash-table-p parsed)
         (error 'tool-input-error
                :message "arguments must be a JSON object with \"prompt\" and \"agent\" or \"id\""))
       parsed))))

(defun %subagent-call (tool run arguments)
  "Start or continue a subagent for RUN; → the tool result text."
  (unless run
    (error 'tool-unavailable :message "subagents need a running parent run"))
  (let* ((args (%subagent-arguments arguments))
         (prompt (%arg args "prompt"))
         (id (%arg args "id"))
         (name (%arg args "agent")))
    (unless prompt
      (error 'tool-input-error :message "\"prompt\" is required"))
    (cond
      (id
       (let ((task (find-subagent-task run id)))
         (unless task
           (error 'tool-input-error
                  :message (format nil "no subagent with id ~s~@[; known ids: ~{~a~^, ~}~]"
                                   id (mapcar #'subagent-task-id
                                              (agent-run-subagents run)))))
         (format nil "~:[Queued the message for~;Continued~] subagent ~a (~a). Do not poll; its reply arrives in your inbox."
                 (eq :continued (continue-subagent task prompt))
                 id (subagent-task-name task))))
      (t
       (let ((agent (find name (subagent-tool-agents tool)
                          :key #'ai-agent-name :test #'equal)))
         (unless agent
           (error 'tool-input-error
                  :message (format nil "unknown agent ~s; choose one of: ~{~a~^, ~}"
                                   name (mapcar #'ai-agent-name
                                                (subagent-tool-agents tool)))))
         (let ((task (start-subagent run agent prompt)))
           (format nil "Started subagent ~a with id ~a. Do not poll or wait; its result arrives in your inbox."
                   (subagent-task-name task) (subagent-task-id task))))))))

(defmethod tool-executable-p ((tool subagent-tool) name &key context)
  (declare (ignore context))
  (equal name (llm-tool-name tool)))

(defmethod tool-permission-resources ((tool subagent-tool) name arguments
                                      &key context)
  "The agent name a start names, or the agent of the task a continue names,
so rules like (\"subagent\" \"reviewer\" :deny) pick which agents may run."
  (declare (ignore name))
  (let* ((args (and (stringp arguments)
                    (let ((parsed (try-parse-json-output arguments)))
                      (and (hash-table-p parsed) parsed))))
         (args (if (hash-table-p arguments) arguments args))
         (run (%context-get context :run))
         (task (and args run (%arg args "id")
                    (find-subagent-task run (%arg args "id")))))
    (list (or (and task (subagent-task-name task))
              (and args (%arg args "agent"))
              "*"))))

(defmethod invoke-tool-async ((tool subagent-tool) name arguments
                              &key context callback error-callback)
  (let ((ok (or callback (lambda (v) (declare (ignore v)))))
        (err (or error-callback #'error)))
    (if (not (equal name (llm-tool-name tool)))
        (%call-on-loop
         (lambda ()
           (funcall err (make-condition 'agent-unknown-tool :name name))))
        (let ((text (handler-case (%subagent-call tool (%context-get context :run)
                                                  arguments)
                      (tool-failure (c)
                        (%call-on-loop (lambda () (funcall err c)))
                        nil))))
          (when text
            (%call-on-loop (lambda () (funcall ok text))))))
    nil))
