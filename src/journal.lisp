(in-package #:ai-agent-protocol)

;;; Run state in the :durability journal (0.7): background subagents, the
;;; inbox and the injected instruction set. Only with a journal that carries
;;; the "agent-state" marker (AGENT-RUN-JOURNAL-STATE non-NIL); an older
;;; journal replays as before.
;;;
;;; Steps (name / idempotency key → result):
;;;   inbox / inbox/post/<n>          (:turn T :task S)  every queued turn, S the
;;;                                   reporting subagent's state or NIL
;;;   inbox / inbox/drain/<gen-key>   (:upto N)  posts delivered before that
;;;                                   generate (only when some were)
;;;   inbox / inbox/continue/<gen-key> T  the run went on after that generate
;;;                                   ended its turn (a report or inbox item)
;;;   instructions / instructions/<gen-key>  directives injected before it
;;;
;;; Subagent tasks come back as the replay passes their subagent tool calls,
;;; then take the state their journaled reports carried. A child that was
;;; :running (or :paused) when the process died cannot be reattached; once the
;;; replay passes the end of the journal it is LOST: marked :failed and the
;;; parent gets a status="failed" report, as for a child that failed.

(defparameter +lost-subagent-text+
  "This subagent was lost: the process running it stopped before it reported, and it cannot be reattached. Its work may be incomplete. Start a new subagent if you still need it."
  "Result and report text of a subagent that was running at a crash.")

;;; --- serial forms ------------------------------------------------------------

(defun %part-state (part)
  (cond
    ((llm-text-part-p part) (list :type :text :text (llm-text-part-text part)))
    ((llm-tool-call-part-p part)
     (list :type :tool-call :id (llm-tool-call-part-id part)
           :name (llm-tool-call-part-name part)
           :arguments (llm-tool-call-part-arguments part)))
    ((llm-tool-result-part-p part)
     (list :type :tool-result :id (llm-tool-result-part-id part)
           :name (llm-tool-result-part-name part)
           :content (llm-tool-result-part-content part)
           :error-p (and (llm-tool-result-part-error-p part) t)))
    ((llm-thinking-part-p part)
     (list :type :thinking :text (llm-thinking-part-text part)
           :signature (llm-thinking-part-signature part)))
    ((llm-image-part-p part)
     (list :type :image :url (llm-image-part-url part)
           :media-type (llm-image-part-media-type part)
           :data (llm-image-part-data part)))
    (t (list :type :text :text (princ-to-string part)))))

(defun %state-part (plist)
  (ecase (getf plist :type)
    (:text (make-llm-text-part :text (getf plist :text)))
    (:tool-call (make-llm-tool-call-part :id (getf plist :id) :name (getf plist :name)
                                         :arguments (or (getf plist :arguments) "{}")))
    (:tool-result (make-llm-tool-result-part :id (getf plist :id) :name (getf plist :name)
                                             :content (getf plist :content)
                                             :error-p (getf plist :error-p)))
    (:thinking (make-llm-thinking-part :text (getf plist :text)
                                       :signature (getf plist :signature)))
    (:image (make-llm-image-part :url (getf plist :url)
                                 :media-type (getf plist :media-type)
                                 :data (getf plist :data)))))

(defun %turn-state (turn)
  (list :role (llm-turn-role turn)
        :parts (mapcar #'%part-state (llm-turn-parts turn))))

(defun %state-turn (plist)
  (make-llm-turn :role (getf plist :role)
                 :parts (mapcar #'%state-part (getf plist :parts))))

(defun %pathname-state (x)
  (if (pathnamep x) (namestring x) x))

(defun %directive-state (d)
  (let ((extra (copy-list (steer:steer-directive-extra d))))
    (when (getf extra :scope)
      (setf (getf extra :scope) (%pathname-state (getf extra :scope))))
    (list :kind (steer:steer-directive-kind d)
          :name (steer:steer-directive-name d)
          :description (steer:steer-directive-description d)
          :body (steer:steer-directive-body d)
          :path (%pathname-state (steer:steer-directive-path d))
          :extra extra)))

(defun %state-directive (plist)
  (let ((extra (copy-list (getf plist :extra)))
        (path (getf plist :path)))
    (when (stringp (getf extra :scope))
      (setf (getf extra :scope) (pathname (getf extra :scope))))
    (steer:make-steer-directive :kind (getf plist :kind)
                                :name (getf plist :name)
                                :description (getf plist :description)
                                :body (getf plist :body)
                                :path (if (stringp path) (pathname path) path)
                                :extra extra)))

(defun %task-state (task)
  (let ((child (subagent-task-run task)))
    (list :id (subagent-task-id task)
          :agent (subagent-task-name task)
          :status (subagent-task-status task)
          :result (subagent-task-result task)
          :turns (and child (eq (subagent-task-status task) :idle)
                      (mapcar #'%turn-state (agent-run-turns child))))))

;;; --- setup ---------------------------------------------------------------------

(defun %prefix-p (prefix s)
  (and (stringp s) (>= (length s) (length prefix))
       (string= prefix s :end2 (length prefix))))

(defun %init-journal-state (run)
  "Read RUN's journaled posts when its journal carries run state."
  (when (and (agent-run-durability run)
             (null (agent-run-journal-state run))
             (%durable-state-version run))
    (let ((posts (loop for (key . result) in (%durable-steps run "inbox")
                       when (%prefix-p "inbox/post/" key) collect result)))
      (setf (agent-run-journal-state run)
            (make-run-journal-state :replay posts :posted (length posts)))))
  run)

;;; --- subagent tasks --------------------------------------------------------------

(defun %find-subagent-agent (run name)
  (dolist (source (agent-run-sources run))
    (when (subagent-tool-p source)
      (let ((agent (find name (subagent-tool-agents source)
                         :key #'ai-agent-name :test #'equal)))
        (when agent (return agent))))))

(defun %restore-child-run (task turns)
  "A finished child AGENT-RUN with TURNS, to continue an idle TASK."
  (let* ((agent (subagent-task-agent task))
         (run (make-agent-run :agent agent
                              :turns turns
                              :settings (coerce-agent-settings (ai-agent-settings agent))
                              :sources (append (ai-agent-tools agent)
                                               (ai-agent-handoffs agent))
                              :session (%agent-session agent)
                              :memory (ai-agent-memory agent)
                              :finish-reason :stop)))
    (setf (agent-run-parent run) (subagent-task-parent task))
    (%init-run-instructions run)
    run))

(defun %restore-task (run id agent)
  (let ((task (make-instance 'subagent-task :id id :agent agent :parent run)))
    (setf (subagent-task-restored-p task) t
          (agent-run-subagents run) (append (agent-run-subagents run) (list task)))
    (%emit run :subagent (list :event :restored :task task))
    task))

(defun %restore-task-state (run state)
  (let* ((id (getf state :id))
         (task (or (find-subagent-task run id)
                   (let ((agent (%find-subagent-agent run (getf state :agent))))
                     (and agent (%restore-task run id agent))))))
    (when task
      (setf (subagent-task-status task) (getf state :status)
            (subagent-task-result task) (getf state :result))
      (when (getf state :turns)
        (setf (subagent-task-run task)
              (%restore-child-run task (mapcar #'%state-turn (getf state :turns))))))
    task))

(defun %replay-tool-state (run inv plist)
  "A journaled subagent tool call replays: bring back the task it started, or
the status a continue gave it."
  (let ((tool (agent-invocation-source inv))
        (text (getf plist :result)))
    (when (and (subagent-tool-p tool) (stringp text) (not (getf plist :error-p)))
      (let ((args (handler-case (%subagent-arguments (agent-invocation-arguments inv))
                    (tool-failure () nil))))
        (when args
          (let ((id (%arg args "id")))
            (if id
                (let ((task (find-subagent-task run id)))
                  (when (and task (%prefix-p "Continued" text))
                    (setf (subagent-task-status task) :running)))
                (let ((agent (find (%arg args "agent") (subagent-tool-agents tool)
                                   :key #'ai-agent-name :test #'equal))
                      (id (%next-subagent-id run)))
                  (when (and agent (search (format nil "id ~a" id) text))
                    (%restore-task run id agent))))))))))

(defun %fail-lost-subagents (run)
  "Restored children still :running or :paused have no process: mark them
:failed and queue a failure report for RUN."
  (dolist (task (agent-run-subagents run))
    (when (and (subagent-task-restored-p task)
               (member (subagent-task-status task) '(:running :paused) :test #'eq))
      (setf (subagent-task-status task) :failed
            (subagent-task-result task) +lost-subagent-text+
            (subagent-task-run task) nil)
      (%emit run :subagent (list :event :lost :task task))
      (%post run (%subagent-report-turn task +lost-subagent-text+ :failed)
             :task task))))

;;; --- inbox ---------------------------------------------------------------------

(defun %journal-post (run turn task)
  "Journal a post to RUN (called with the inbox lock held)."
  (let ((state (agent-run-journal-state run)))
    (when state
      (%durable-record-step run "inbox"
                            (list :turn (%turn-state turn)
                                  :task (and task (%task-state task)))
                            :idempotency-key (format nil "inbox/post/~d"
                                                     (run-journal-posted state)))
      (incf (run-journal-posted state)))))

(defun %apply-post (run post)
  (when (getf post :task)
    (%restore-task-state run (getf post :task)))
  (%state-turn (getf post :turn)))

(defun %flush-replay-posts (run)
  "Journaled posts the replay has not delivered go back into RUN's inbox,
ahead of anything queued since."
  (let* ((state (agent-run-journal-state run))
         (posts (and state (shiftf (run-journal-replay state) nil))))
    (when posts
      (let ((turns (mapcar (lambda (p) (%apply-post run p)) posts)))
        (bt:with-lock-held ((agent-run-inbox-lock run))
          (setf (agent-run-inbox run) (append turns (agent-run-inbox run))))))))

(defun %journaled-drain (run)
  "→ inbox turns for the generate about to run. Within the journal: the posts
it delivered there. Past it: everything queued, after lost children report."
  (let* ((state (agent-run-journal-state run))
         (gen-key (%generate-step-key run))
         (key (format nil "inbox/drain/~a" gen-key)))
    (multiple-value-bind (hit record) (%durable-peek run "inbox" key)
      (cond
        (hit
         (let ((n (- (getf record :upto) (run-journal-drained state))))
           (setf (run-journal-drained state) (getf record :upto))
           (loop repeat n
                 while (run-journal-replay state)
                 collect (%apply-post run (pop (run-journal-replay state))))))
        ((%durable-peek run "generate" gen-key)
         ;; The journaled run delivered nothing before this generate.
         nil)
        (t
         (%flush-replay-posts run)
         (%fail-lost-subagents run)
         (let ((turns (bt:with-lock-held ((agent-run-inbox-lock run))
                        (shiftf (agent-run-inbox run) nil))))
           (when turns
             (incf (run-journal-drained state) (length turns))
             (%durable-record-step run "inbox"
                                   (list :upto (run-journal-drained state))
                                   :idempotency-key key))
           turns))))))

;;; --- instructions ------------------------------------------------------------

(defun %journaled-instructions (run)
  "→ (values handled directives). HANDLED when the journal decides what is
injected before this generate: what it recorded, or nothing."
  (when (agent-run-journal-state run)
    (let ((gen-key (%generate-step-key run)))
      (multiple-value-bind (hit record)
          (%durable-peek run "instructions" (format nil "instructions/~a" gen-key))
        (cond
          (hit (values t (mapcar #'%state-directive record)))
          ((%durable-peek run "generate" gen-key) (values t nil))
          (t (values nil nil)))))))

(defun %journal-instructions (run directives)
  (when (and directives (agent-run-journal-state run))
    (%durable-record-step run "instructions" (mapcar #'%directive-state directives)
                          :idempotency-key (format nil "instructions/~a"
                                                   (%generate-step-key run)))))
