(in-package #:ai-agent-protocol)

;;; Default RUN / RESUME. Generate and sync tool handlers run %off-loop.

(defun %backend (run)
  (or (ai-agent-backend (agent-run-agent run))
      llm-protocol:*llm-backend*
      (error 'agent-error :message "no llm-backend on agent or *llm-backend*")))

(defun %run-model (run)
  "Model id passed to the backend: the run settings' :model, else the running
agent's AI-AGENT-MODEL. NIL → the backend's default model."
  (or (agent-settings-model (agent-run-settings run))
      (ai-agent-model (agent-run-agent run))))

(defun %plist-get (plist key)
  (and (consp plist) (getf plist key)))

(defun %tool-choice (run)
  "TOOL-CHOICE for this GENERATE. EXTRA :tool-choice always; :first-tool-choice
   only on step 1 (so a follow-up after a tool result is not forced)."
  (let* ((settings (agent-run-settings run))
         (extra (agent-settings-extra settings))
         (llm (agent-settings-llm settings)))
    (when (agent-run-wrap-up-p run)
      ;; Tools stay in the request (cached prefix intact); none may be called.
      (return-from %tool-choice :none))
    (or (%plist-get extra :tool-choice)
        (and (= 1 (agent-run-step run))
             (%plist-get extra :first-tool-choice))
        (and (llm-settings-p llm)
             (%plist-get (llm-settings-extra llm) :tool-choice)))))

(defun %canceled-p (run)
  (agent-run-handle-canceled-p (agent-run-handle run)))

(defun %terminal-finish-p (reason)
  (not (member reason '(:approval :deferred) :test #'eq)))

(defun %remember-run (run)
  (let ((mem (or (agent-run-memory run)
                 (ai-agent-memory (agent-run-agent run)))))
    (when mem
      (conversation:remember mem (agent-run-turns run)
                             :session (or (agent-run-session run)
                                          (%agent-session (agent-run-agent run)))
                             :replace t))))

(defun %part-plist (part)
  "Journal form of a generated PART (LLM-PROTOCOL:PART-STATE, lossless)."
  (part-state part))

(defun %plist-part (plist)
  "Read back %PART-PLIST. A part no loaded system knows (and the :unknown
records journals before 0.9 wrote for thinking and provider parts) replays as
empty text, as it always did."
  (or (part-from-state plist)
      (make-llm-text-part :text "")))

(defun %response-plist (response)
  (list :finish-reason (llm-response-finish-reason response)
        :model (llm-response-model response)
        :parts (mapcar #'%part-plist (copy-list (llm-response-parts response)))))

(defun %plist-response (plist)
  (make-llm-response
   :finish-reason (or (getf plist :finish-reason) :stop)
   :model (getf plist :model)
   :parts (mapcar #'%plist-part (getf plist :parts))))

(defun %generate-step-key (run)
  ;; /length-retry: the regenerate of a truncated tool call (0.16.0 rule 4a)
  ;; must not replay the truncated response journaled under the plain key.
  (%segment-key run (format nil "generate/~d~:[~;/wrap-up~]~:[~;/length-retry~]"
                            (agent-run-step run)
                            (agent-run-wrap-up-p run)
                            (%length-retry-step-p run))))

(defun %extra (run key &optional default)
  "KEY of the run's AGENT-SETTINGS-EXTRA plist, or DEFAULT when absent."
  (let ((extra (agent-settings-extra (agent-run-settings run))))
    (if (listp extra) (getf extra key default) default)))

(defparameter +max-steps-prompt+
  "You have reached the maximum number of steps for this task, and tools are now disabled. Reply with what you accomplished, what remains to be done, and the next step you would take."
  "User turn added for the tool-less wrap-up generate at max steps
(override per run with settings extra :max-steps-prompt).")

(defun %tool-step-key (run inv)
  ;; Segmented: local models reuse call ids (call_0) across continuations.
  (%segment-key run (format nil "tool/~a" (agent-invocation-id inv))))

(defun %finish (run reason callback)
  (when (member reason '(:approval :deferred) :test #'eq)
    (%durable-wait-input
     run
     (list :reason reason
           :pending (mapcar (lambda (inv)
                              (list :id (agent-invocation-id inv)
                                    :name (agent-invocation-name inv)
                                    :arguments (agent-invocation-arguments inv)
                                    :status (agent-invocation-status inv)))
                            (agent-run-pending run)))))
  (setf (agent-run-finish-reason run) reason)
  (when (%terminal-finish-p reason)
    (%remember-run run)
    ;; Steering posted during the last step was not delivered: say so. It
    ;; stays queued (CONTINUE delivers it) unless taken with
    ;; DRAIN-AGENT-RUN-INBOX.
    (let ((undelivered (%undelivered-inbox run)))
      (when undelivered
        (%emit run :undelivered undelivered))))
  (%emit run :finished run)
  (funcall callback run)
  run)

(defun %check-canceled (run callback)
  "T when the run finished as :canceled. CONTINUE skips the finish."
  (when (%canceled-p run)
    (let ((act (restart-case
                   (progn
                     (signal 'agent-canceled :run run :message "agent run canceled")
                     :finish)
                 (continue ()
                   :report "Ignore cancel and continue the run"
                   (progn :continue))
                 (abort ()
                   :report "Finish the run as canceled"
                   (progn :finish)))))
      (when (eq act :finish)
        (%settle-in-flight run)
        (%finish run :canceled callback)
        t))))

(defun %settle-in-flight (run)
  "A cancel ends the step: the partial assistant turn stays in history as
what the user saw, minus its tool calls (they get no results), and is no
longer in flight, so the next step opens its own turn (#29). A turn left
with no parts is dropped."
  (let ((turn (agent-run-in-flight-turn run)))
    (when turn
      (setf (agent-run-in-flight-turn run) nil
            (llm-turn-parts turn)
            (remove-if #'llm-tool-call-part-p (llm-turn-parts turn)))
      (unless (llm-turn-parts turn)
        (setf (agent-run-turns run)
              (remove turn (agent-run-turns run) :test #'eq))))))

(defun %handoff-for (run name)
  (find name (ai-agent-handoffs (agent-run-agent run))
        :key #'ai-agent-name :test #'equal))

(defun %find-source (run name)
  (let ((sources (agent-run-sources run)))
    (or (find-if (lambda (s)
                   (some (lambda (d) (equal name (llm-tool-name d)))
                         (list-agent-tools s)))
                 sources)
        ;; 0.17.0: a name no source lists goes to the first source that
        ;; executes it (a deferred tool called before it is listed).
        (find-if (lambda (s) (tool-executable-p s name)) sources)
        (%handoff-for run name))))

(defun %emit-invocation (run inv)
  "Emit :invocation for statuses the transformer / TUI care about."
  (when (member (agent-invocation-status inv)
                '(:approved :running :done :error :denied) :test #'eq)
    (%emit run :invocation inv)))

(defun %merge-part-into-turn (turn part)
  "Append PART. Same-id tool-call parts concatenate ARGUMENTS (delta suffix).
   Copy tool-call parts so we do not mutate the backend's response objects."
  (cond
    ((llm-tool-call-part-p part)
     (let* ((id (llm-tool-call-part-id part))
            (existing (find-if (lambda (p)
                                 (and (llm-tool-call-part-p p)
                                      (equal (llm-tool-call-part-id p) id)))
                               (llm-turn-parts turn))))
       (if existing
           (setf (llm-tool-call-part-arguments existing)
                 (concatenate 'string
                              (or (llm-tool-call-part-arguments existing) "")
                              (or (llm-tool-call-part-arguments part) "")))
           (setf (llm-turn-parts turn)
                 (append (llm-turn-parts turn)
                         (list (make-llm-tool-call-part
                                :id id
                                :name (llm-tool-call-part-name part)
                                :arguments (or (llm-tool-call-part-arguments part) ""))))))))
    (t
     (setf (llm-turn-parts turn)
           (append (llm-turn-parts turn) (list part))))))

(defun %append-in-flight-part (run part)
  "First :part of a step opens an assistant turn; later parts merge into it."
  (let ((turn (agent-run-in-flight-turn run)))
    (unless turn
      (setf turn (make-llm-turn :role :assistant :parts nil)
            (agent-run-in-flight-turn run) turn
            (agent-run-turns run) (append (agent-run-turns run) (list turn))))
    (%merge-part-into-turn turn part)))

(defun %coalesce-parts (parts)
  "Merge same-id tool-call parts (argument suffixes) into one part each."
  (let ((out '())
        (tools (make-hash-table :test #'equal)))
    (dolist (p parts)
      (if (llm-tool-call-part-p p)
          (let* ((id (llm-tool-call-part-id p))
                 (ex (gethash id tools)))
            (if ex
                (setf (llm-tool-call-part-arguments ex)
                      (concatenate 'string
                                   (or (llm-tool-call-part-arguments ex) "")
                                   (or (llm-tool-call-part-arguments p) "")))
                (let ((copy (make-llm-tool-call-part
                             :id id
                             :name (llm-tool-call-part-name p)
                             :arguments (or (llm-tool-call-part-arguments p) ""))))
                  (setf (gethash id tools) copy)
                  (push copy out))))
          (push p out)))
    (nreverse out)))

(defun %commit-assistant-turn (run response)
  "RESPONSE parts are source of truth. Do not append a second assistant turn."
  (let ((existing (agent-run-in-flight-turn run))
        (parts (%coalesce-parts (copy-list (llm-response-parts response)))))
    (setf (agent-run-in-flight-turn run) nil)
    (if existing
        (setf (llm-turn-parts existing) parts)
        (setf (agent-run-turns run)
              (append (agent-run-turns run)
                      (list (make-llm-turn :role :assistant :parts parts)))))))

(defun %committed-tool-calls (run)
  "Tool-call parts from the committed assistant turn (same-id suffixes already merged)."
  (let ((turn (find :assistant (agent-run-turns run) :key #'llm-turn-role :from-end t)))
    (and turn (remove-if-not #'llm-tool-call-part-p (llm-turn-parts turn)))))

(defun %append-invocation-turn (run inv)
  (unless (agent-invocation-recorded-p inv)
    (let ((turn (tool-turn (agent-invocation-id inv)
                           (or (agent-invocation-result inv) "")
                           :name (agent-invocation-name inv)
                           :error-p (agent-invocation-error-p inv))))
      (setf (agent-run-turns run) (append (agent-run-turns run) (list turn))
            (agent-invocation-recorded-p inv) t)
      (when (agent-invocation-parts inv)
        (setf (%agent-run-rich-tool-turns run)
              (append (%agent-run-rich-tool-turns run) (list (cons turn inv))))))))

(defun %rich-output-turn (invs)
  "One :user turn: per invocation of INVS, in order, a text part
\"[output of tool NAME (ID)]\" followed by its parts (the same objects)."
  (make-llm-turn
   :role :user
   :parts (loop for inv in invs
                collect (make-llm-text-part
                         :text (format nil "[output of tool ~a (~a)]"
                                       (agent-invocation-name inv)
                                       (agent-invocation-id inv)))
                append (copy-list (agent-invocation-parts inv)))))

(defun %generate-turns (run)
  "The turns GENERATE gets: AGENT-RUN-TURNS, plus one :user turn right after
each run of consecutive tool turns that holds a rich invocation's result
(AGENT-INVOCATION-PARTS). Tool results stay text; history, journals and memory
never hold the parts. A run without rich output gets AGENT-RUN-TURNS itself."
  (let ((rich (%agent-run-rich-tool-turns run)))
    (if (null rich)
        (agent-run-turns run)
        (let ((out '())
              (block '()))
          (flet ((flush ()
                   (when block
                     (push (%rich-output-turn (nreverse block)) out)
                     (setf block '()))))
            (dolist (turn (agent-run-turns run))
              (unless (eq :tool (llm-turn-role turn))
                (flush))
              (push turn out)
              (let ((cell (assoc turn rich :test #'eq)))
                (when cell
                  (push (cdr cell) block))))
            (flush)
            (nreverse out))))))

(defun %record-terminal-invocations (run)
  (dolist (inv (agent-run-invocations run))
    (when (member (agent-invocation-status inv)
                  '(:done :denied :error) :test #'eq)
      (%append-invocation-turn run inv))))

(defun %switch-handoff (run specialist)
  (let ((sys (ai-agent-instructions specialist)))
    (setf (agent-run-agent run) specialist
          (agent-run-sources run) (append (ai-agent-tools specialist)
                                          (ai-agent-handoffs specialist)
                                          (copy-list (agent-run-extra run)))
          (agent-run-turns run)
          (cons (system-turn (or sys (ai-agent-name specialist)))
                (remove :system (agent-run-turns run) :key #'llm-turn-role)))
    (%emit run :handoff specialist)))

(defun %unknown-tool-text (run name)
  (let ((names (loop for d in (collect-run-tools (agent-run-agent run)
                                                 :extra (agent-run-extra run))
                     when (llm-tool-p d) collect (llm-tool-name d))))
    (format nil "Unknown tool ~s. Call only the tools you were given~@[: ~{~a~^, ~}~]."
            name (subseq names 0 (min 40 (length names))))))

(defun %classify (run call)
  (let* ((name (llm-tool-call-part-name call))
         (args (or (llm-tool-call-part-arguments call) "{}"))
         (source (%find-source run name))
         (inv (make-agent-invocation
               :id (or (llm-tool-call-part-id call)
                       (format nil "call-~a" (random (expt 36 8))))
               :name name :arguments args :source source)))
    (cond
      ((%handoff-for run name)
       (setf (agent-invocation-status inv) :handoff
             (agent-invocation-source inv) (%handoff-for run name)))
      ((null source)
       (let ((act (restart-case
                      (progn
                        (signal 'agent-unknown-tool
                                :run run :name name
                                :message (format nil "unknown tool ~s" name))
                        :skip)
                    (use-value (result)
                      :report "Use a supplied tool result string"
                      (setf (agent-invocation-status inv) :done
                            (agent-invocation-result inv)
                            (if (stringp result) result (princ-to-string result)))
                      :used)
                    (skip-tool ()
                      :report "Record the unknown tool as an error and continue"
                      (progn :skip)))))
         (when (eq act :skip)
           (setf (agent-invocation-status inv) :error
                 (agent-invocation-error-p inv) t
                 (agent-invocation-result inv) (%unknown-tool-text run name)))))
      ((%permission-denied-p run inv source))
      ((not (tool-executable-p source name))
       (setf (agent-invocation-status inv) :deferred))
      ((agent-approve-p (agent-run-agent run) source name args
                        :context (%permission-context run))
       (setf (agent-invocation-status inv) :proposed))
      (t
       (setf (agent-invocation-status inv) :approved)))
    (%emit-invocation run inv)
    (%apply-journaled-hitl run inv)
    inv))

(defun %read-approval-feedback ()
  (format *query-io* "Feedback for the model (empty for none): ")
  (force-output *query-io*)
  (let ((line (read-line *query-io*)))
    (and (plusp (length line)) (list line))))

(defun %deny-offered (run inv feedback &optional (by :user))
  (if (or feedback (not (eq by :user)))
      (%refuse-invocation run inv (%rejection inv feedback by))
      (setf (agent-invocation-status inv) :denied
            (agent-invocation-result inv) "denied"
            (agent-invocation-error-p inv) nil))
  (%journal-denial run inv)
  (%emit-invocation run inv))

(defun %approve-offered (run inv &optional rules)
  (setf (agent-invocation-status inv) :approved)
  (%journal-approval run inv rules)
  (%emit-invocation run inv))

(defun %offer-approval (run inv)
  (when (eq (agent-invocation-status inv) :proposed)
    (restart-case
        (signal 'agent-approval-required :run run :invocation inv)
      (approve ()
        :report "Approve the tool invocation"
        (%approve-offered run inv))
      (approve-always ()
        :report "Approve it and allow matching calls for the rest of the run"
        (let ((rules (%always-rules run inv)))
          (setf (agent-invocation-status inv) :approved)
          (unwind-protect
               (setf rules (%remember-approval run inv rules))
            (%approve-offered run inv rules))
          (when rules
            (dolist (other (%proposed-invocations run))
              (when (%now-allowed-p run other)
                (%approve-offered run other))))))
      (deny (&optional feedback &key (by :user))
        :report "Deny the tool invocation"
        :interactive %read-approval-feedback
        (%deny-offered run inv feedback by))
      (deny-all (&optional feedback &key (by :user))
        :report "Deny it and every other pending invocation of the run"
        :interactive %read-approval-feedback
        (dolist (i (%proposed-invocations run))
          (%deny-offered run i feedback by)))
      (pause-for-approval ()
        :report "Pause the run for HITL approval"
        nil))))

(defun %after-invocations (run callback error-callback)
  (dolist (inv (agent-run-invocations run))
    (when (eq (agent-invocation-status inv) :proposed)
      (%offer-approval run inv)))
  ;; After the approval replies: a call a handler denied is terminal now and
  ;; needs its tool result before the next GENERATE (#27).
  (%record-terminal-invocations run)
  (let ((pending (remove-if-not
                  (lambda (i)
                    (member (agent-invocation-status i)
                            '(:proposed :deferred) :test #'eq))
                  (agent-run-invocations run)))
        (ready (remove-if-not
                (lambda (i)
                  (and (eq (agent-invocation-status i) :approved)
                       (not (agent-invocation-recorded-p i))))
                (agent-run-invocations run)))
        ;; 0.16.1 (#51): a done call whose output said :END-RUN
        (enders (remove-if-not #'agent-invocation-end-run-p
                               (agent-run-invocations run))))
    (setf (agent-run-pending run) pending)
    (cond
      ((%check-canceled run callback))
      (ready
       (%invoke-ready run ready callback error-callback))
      (pending
       (%finish run
                (if (find :deferred pending :key #'agent-invocation-status)
                    :deferred
                    :approval)
                callback))
      (enders
       ;; Every call of the step is recorded: no model step follows. The
       ;; flag is spent, so a CONTINUE of this run generates again.
       (dolist (i enders) (setf (agent-invocation-end-run-p i) nil))
       (%finish run :tool-end callback))
      (t
       (%tick-run run callback error-callback)))))

(defun %invoke-ready (run invs callback error-callback)
  (if (null invs)
      (%after-invocations run callback error-callback)
      (let ((lock (bt:make-lock "ai-agent-join"))
            (left (length invs)))
        (flet ((one-done ()
                 (bt:with-lock-held (lock)
                   (decf left)
                   (when (zerop left)
                     (%call-on-loop
                      (lambda ()
                        (%after-invocations run callback error-callback)))))))
          (dolist (inv invs)
            (setf (agent-invocation-status inv) :running)
            (%emit-invocation run inv)
            (%invoke-one-tool run inv #'one-done))))))

(defun %apply-tool-plist (run inv plist)
  (setf (agent-invocation-status inv) (or (getf plist :status) :done)
        (agent-invocation-result inv) (getf plist :result)
        (agent-invocation-error-p inv) (getf plist :error-p)
        ;; 0.16.1 (#51): live and replayed alike
        (agent-invocation-end-run-p inv) (and (getf plist :end-run) t))
  (%emit-invocation run inv))

(defun %record-tool-failure (run inv condition)
  "Record INV as failed with CONDITION, the error its handler signalled. A
refusal (TOOL-FAILURE-KIND :DENIED, e.g. a source's own TOOL-PERMISSION-DENIED)
is recorded like a rule denial: status :DENIED and a :PERMISSION event."
  (if (eq (tool-failure-kind condition) :denied)
      (%refuse-invocation run inv condition)
      (setf (agent-invocation-status inv) :error
            (agent-invocation-error-p inv) t
            (agent-invocation-result inv)
            (tool-failure-text condition (agent-invocation-name inv)))))

(defun %tool-output-values (result)
  "→ (values TEXT PARTS STRUCTURED END-RUN) of a tool CALLBACK value: a string,
an AGENT-TOOL-OUTPUT, or anything else (printed)."
  (typecase result
    (string (values result nil nil nil))
    (agent-tool-output (values (or (agent-tool-output-text result) "")
                               (agent-tool-output-parts result)
                               (agent-tool-output-structured result)
                               (agent-tool-output-end-run-p result)))
    (t (values (princ-to-string result) nil nil nil))))

(defun %invoke-one-tool (run inv done)
  (let ((key (%tool-step-key run inv)))
    (multiple-value-bind (hit plist)
        (%durable-step-replayed run "tool" :idempotency-key key)
      (cond
        (hit
         (when (agent-run-journal-state run)
           (%replay-tool-state run inv plist))
         (%apply-tool-plist run inv plist)
         (funcall done))
        (t
         (invoke-tool-async (agent-invocation-source inv)
                            (agent-invocation-name inv)
                            (agent-invocation-arguments inv)
                            :context (list :run run :invocation inv)
                            :callback (lambda (result)
                                        (multiple-value-bind (text parts structured end-run)
                                            (%tool-output-values result)
                                          ;; Journals record TEXT only, plus the end-run flag (0.16.1).
                                          (let ((plist (list* :status :done
                                                              :result text
                                                              :error-p nil
                                                              (and end-run (list :end-run t)))))
                                            (%durable-record-step run "tool" plist
                                                                  :idempotency-key key)
                                            (setf (agent-invocation-parts inv) parts
                                                  (agent-invocation-structured inv) structured)
                                            (%apply-tool-plist run inv plist)
                                            (funcall done))))
                            :error-callback (lambda (c)
                                              (let ((act
                                                     (restart-case
                                                         (progn
                                                           (signal 'agent-tool-error
                                                                   :run run
                                                                   :invocation inv
                                                                   :cause c
                                                                   :name (agent-invocation-name inv)
                                                                   :message (princ-to-string c))
                                                           :skip)
                                                       (use-value (result)
                                                         :report "Use a supplied tool result string"
                                                         (setf (agent-invocation-status inv) :done
                                                               (agent-invocation-error-p inv) nil
                                                               (agent-invocation-result inv)
                                                               (if (stringp result)
                                                                   result
                                                                   (princ-to-string result)))
                                                         :used)
                                                       (skip-tool ()
                                                         :report "Record the tool error and continue"
                                                         (progn :skip)))))
                                                (when (eq act :used)
                                                  (%durable-record-step
                                                   run "tool"
                                                   (list :status :done
                                                         :result (agent-invocation-result inv)
                                                         :error-p nil)
                                                   :idempotency-key key)
                                                  (%emit-invocation run inv))
                                                (when (eq act :skip)
                                                  (%record-tool-failure run inv c)
                                                  (%durable-record-step
                                                   run "tool"
                                                   (list :status (agent-invocation-status inv)
                                                         :result (agent-invocation-result inv)
                                                         :error-p t)
                                                   :idempotency-key key)
                                                  (%emit-invocation run inv))
                                                (funcall done)))))))))

(defun %on-generate (run response callback error-callback)
  (handler-case
      (progn
        (when (%check-canceled run callback)
          (return-from %on-generate))
        ;; 0.16.0 rule 4a: a cut-off tool call → drop RESPONSE before commit
        ;; and regenerate the step with more output tokens.
        (when (%length-retry-p run response)
          (return-from %on-generate
            (%retry-truncated run callback error-callback)))
        (setf (agent-run-last-response run) response)
        (%commit-assistant-turn run response)
        (setf (agent-run-usage-anchor run) (length (agent-run-turns run))
              (agent-run-generate-attempt run) 1)
        (%track-continuation run response)
        (%emit run :response response)
        (when (agent-run-wrap-up-p run)
          (%strip-tool-calls run)
          (return-from %on-generate (%finish run :max-steps callback)))
        (let ((calls (%committed-tool-calls run))
              (reason (llm-response-finish-reason response)))
          (cond
            ((and (null calls) (%continue-output-p run reason))
             (%continue-output run response callback error-callback))
            ((null calls)
             (%finish-or-await-subagents run (or reason :stop)
                                         callback error-callback))
            (t
             (let* ((cut (and (eq reason :length) (car (last calls))))
                    (invs (append (mapcar (lambda (c) (%classify run c))
                                          (if cut (butlast calls) calls))
                                  (and cut (list (%truncated-invocation run cut))))))
               (setf (agent-run-invocations run)
                     (append (agent-run-invocations run) invs))
               (let ((handoff (find :handoff invs :key #'agent-invocation-status)))
                 (if handoff
                     (progn
                       (%switch-handoff run (agent-invocation-source handoff))
                       (setf (agent-invocation-status handoff) :done
                             (agent-invocation-result handoff)
                             (format nil "handed off to ~a"
                                     (ai-agent-name (agent-invocation-source handoff))))
                       (%emit-invocation run handoff)
                       (%append-invocation-turn run handoff)
                       (%tick-run run callback error-callback))
                     (let ((ready (remove-if-not
                                   (lambda (i)
                                     (eq (agent-invocation-status i) :approved))
                                   invs)))
                       (%invoke-ready run ready callback error-callback)))))))))
    (error (e)
      (funcall error-callback e))))

(defun %do-generate (run callback error-callback)
  (let ((key (%generate-step-key run)))
    (multiple-value-bind (hit plist)
        (%durable-step-replayed run "generate" :idempotency-key key)
      (when hit
        (%on-generate run (%plist-response plist) callback error-callback)
        (return-from %do-generate)))
    (let ((choice (%tool-choice run))
          ;; On the loop, before the worker starts: rich tool output's :user turns.
          (turns (%generate-turns run))
          ;; 0.16.0 rule 4a: a copy with raised max tokens on a length retry.
          (llm-settings (%step-llm-settings run)))
      (multiple-value-bind (eb el) (%event-context)
        (%off-loop
         (lambda ()
           (flet ((emit-part (part)
                    (let ((p part))
                      ;; Capture EB/EL — worker has no *event-loop* binding.
                      (event:wake-call eb el
                                       (lambda ()
                                         (%append-in-flight-part run p)
                                         (%emit run :part p))))))
             (handler-case
                 (stream-generate (%backend run)
                                  turns
                                  :model (%run-model run)
                                  :settings llm-settings
                                  :tools (collect-run-tools (agent-run-agent run)
                                                            :extra (agent-run-extra run))
                                  :tool-choice choice
                                  :on-part #'emit-part)
               (llm-unsupported ()
                 (let ((response (generate (%backend run)
                                           turns
                                           :model (%run-model run)
                                           :settings llm-settings
                                           :tools (collect-run-tools (agent-run-agent run)
                                                                     :extra (agent-run-extra run))
                                           :tool-choice choice)))
                   (dolist (part (llm-response-parts response))
                     (emit-part part))
                   response)))))
         (lambda (response)
           (%durable-record-step run "generate" (%response-plist response)
                                 :idempotency-key key)
           (%on-generate run response callback error-callback))
         (lambda (c)
           (let ((act (restart-case
                          (progn
                            (signal 'agent-generate-error
                                    :run run
                                    :step (agent-run-step run)
                                    :cause c
                                    :message (princ-to-string c))
                            :fail)
                        (retry ()
                          :report "Retry GENERATE"
                          (progn :retry))
                        (use-value (response)
                          :report "Use a supplied LLM-RESPONSE"
                          (%durable-record-step run "generate"
                                                (%response-plist response)
                                                :idempotency-key key)
                          (%on-generate run response callback error-callback)
                          :used)
                        (abort ()
                          :report "Fail the generate step"
                          (progn :fail)))))
             (cond
               ((eq act :retry)
                (%discard-in-flight run)
                (%do-generate run callback error-callback))
               ((eq act :fail)
                (%recover-generate run c callback error-callback))))))))))

(defun %discard-in-flight (run)
  "Drop the partial assistant turn a failed streaming step left behind."
  (let ((turn (agent-run-in-flight-turn run)))
    (when turn
      (setf (agent-run-turns run) (remove turn (agent-run-turns run) :test #'eq)
            (agent-run-in-flight-turn run) nil))))

(defun %strip-tool-calls (run)
  "The wrap-up reply may not call tools: drop any calls it made anyway, so
history never holds a call without a result."
  (let ((turn (find :assistant (agent-run-turns run) :key #'llm-turn-role
                                                     :from-end t)))
    (when turn
      (setf (llm-turn-parts turn)
            (or (remove-if #'llm-tool-call-part-p (llm-turn-parts turn))
                (list (make-llm-text-part :text "(max steps reached)")))))))

(defun %recover-generate (run c callback error-callback)
  "No handler recovered generate failure C. Compact once on context overflow;
retry what the retry policy covers after its delay (on the loop, without
blocking it); otherwise fail the run."
  (let ((step (agent-run-step run)))
    (cond
      ((and (typep c 'llm-context-overflow)
            (%compactor run)
            (not (eql (agent-run-compacted-step run) step)))
       (setf (agent-run-compacted-step run) step)
       (%discard-in-flight run)
       (%emit run :retry (%retry-payload :context-overflow c
                                         (agent-run-generate-attempt run) 0))
       (%maybe-compact run
                       (lambda (did)
                         (if did
                             (%generate-with-instructions run callback error-callback)
                             (funcall error-callback c)))
                       error-callback
                       :force t :reason :overflow))
      (t
       (let* ((policy (%extra run :retry-policy *retry-policy*))
              (attempt (agent-run-generate-attempt run))
              (delay (retry-delay policy c attempt)))
         (if (null delay)
             (funcall error-callback c)
             (progn
               (setf (agent-run-generate-attempt run) (1+ attempt))
               (%discard-in-flight run)
               ;; 0.16.0 rule 1: park the wait first, so a cancel — even one
               ;; made from inside the :retry emit — ends it at once.
               (%backoff run delay
                         (lambda () (%do-generate run callback error-callback))
                         callback)
               (%emit run :retry (%retry-payload (llm-retry-class c) c attempt
                                                 delay policy)))))))))

;;; --- agent-side compaction -------------------------------------------------
;;; Settings extra :compaction is a conversation memory (e.g. summary-memory);
;;; COMPACT-TURNS on it shrinks the run's turns. It runs before a step whose
;;; prompt (measured usage + estimate) exceeds :compact-at, or the model's
;;; context window less the completion reserve, and once on context overflow.
;;; API-side compaction is the backend's job (llm-protocol-anthropic
;;; :server-compaction); the two can be combined.

(defun %compactor (run)
  (%extra run :compaction))

(defun %compact-limit (run backend)
  (or (%extra run :compact-at)
      (let ((window (context-window backend (%run-model run)))
            (llm (coerce-settings (agent-settings-llm (agent-run-settings run)))))
        (and window
             (- window (or (and llm (llm-settings-max-tokens llm)) 4096))))))

(defun %compact-step-key (run force)
  (%segment-key run (format nil "compact/~d~:[~;/overflow~]"
                            (agent-run-step run) force)))

(defun %compaction-record (old new)
  "Journal form of a compaction OLD → NEW: the turns NEW kept from the end of
OLD (:keep, by identity) and the new turns before them (:head, e.g. the
summary)."
  (let ((keep (loop for a in (reverse old)
                    for b in (reverse new)
                    while (eq a b)
                    count t)))
    (list :did t
          :keep keep
          :head (mapcar #'%turn-state (butlast new keep)))))

(defun %compaction-turns (old record)
  "OLD's turns after replaying compaction RECORD."
  (append (mapcar #'%state-turn (getf record :head))
          (last old (getf record :keep))))

(defun %maybe-compact (run next error-callback
                       &key force (reason (if force :overflow :threshold))
                         instructions key)
  "Call NEXT with T after compacting the run's turns, or NIL when nothing was
compacted. Counting and summarizing run off the loop. Under :durability the
outcome is journaled and replayed verbatim: no second summarizer call, and the
replayed history is the one the run had (#17). REASON goes to the :COMPACTED
event (:threshold, :overflow or :manual); INSTRUCTIONS are added to the
summary prompt for this call only; KEY overrides the journal key (0.16.0)."
  (let ((mem (%compactor run)))
    (if (null mem)
        (funcall next nil)
        (let* ((turns (agent-run-turns run))
               (backend (%backend run))
               (anchor (agent-run-usage-anchor run))
               (response (agent-run-last-response run))
               (usage (and anchor response (llm-response-usage response)))
               (key (or key (%compact-step-key run force))))
          (flet ((apply-compacted (new keep tokens)
                   (setf (agent-run-turns run) new
                         (agent-run-usage-anchor run) nil)
                   (%instructions-compacted run)
                   (%emit run :compacted (list :before (length turns)
                                               :after (length new)
                                               :reason reason
                                               :summary (butlast new keep)
                                               :keep keep
                                               :tokens-before tokens))))
            (multiple-value-bind (hit record)
                (%durable-step-replayed run "compact" :idempotency-key key)
              (when hit
                (let ((did (getf record :did)))
                  (when did
                    ;; Records before 0.16.0 have no :tokens-before → NIL.
                    (apply-compacted (%compaction-turns turns record)
                                     (getf record :keep)
                                     (getf record :tokens-before)))
                  (return-from %maybe-compact (funcall next did)))))
            (%off-loop
             (lambda ()
               (let* ((limit (and (not force) (%compact-limit run backend)))
                      (tokens (and limit
                                   (anchored-token-count backend turns
                                                         :usage usage
                                                         :anchor-count (or anchor 0)))))
                 (when (or force (and tokens (> tokens limit)))
                   (cons tokens
                         (multiple-value-list
                          (%compact-turns-with mem turns backend instructions))))))
             (lambda (result)
               (destructuring-bind (&optional tokens new did) result
                 (let* ((did (and did
                                  ;; A manual compaction raced by a continue:
                                  ;; the result no longer matches the turns.
                                  (or (not (eq reason :manual))
                                      (eq turns (agent-run-turns run)))
                                  t))
                        (record (if did
                                    (list* :reason reason :tokens-before tokens
                                           (%compaction-record turns new))
                                    (list :did nil))))
                   (%durable-record-step run "compact" record :idempotency-key key)
                   (when did
                     (apply-compacted new (getf record :keep) tokens))
                   (funcall next did))))
             error-callback))))))

(defun %wrap-up-or-finish (run callback error-callback)
  "At max steps: one more GENERATE with tools disabled so the run ends with a
summary instead of a dangling tool loop (settings extra :max-steps-wrap-up nil
turns it off), then finish :max-steps."
  (if (or (agent-run-wrap-up-p run)
          (not (%extra run :max-steps-wrap-up t)))
      (%finish run :max-steps callback)
      (progn
        ;; Wrap-up first: drain and injection journal under the wrap-up key.
        (setf (agent-run-wrap-up-p run) t)
        (%drain-inbox run)
        (unless (%inject-instructions-or-fail run error-callback)
          (return-from %wrap-up-or-finish))
        (setf (agent-run-turns run)
              (append (agent-run-turns run)
                      (list (user-turn (%extra run :max-steps-prompt
                                               +max-steps-prompt+)))))
        (%emit run :wrap-up (agent-run-step run))
        (%do-generate run callback error-callback))))

(defun %tick-run (run callback error-callback)
  (when (%check-canceled run callback)
    (return-from %tick-run))
  (let ((max (or (agent-settings-max-steps (agent-run-settings run)) 20)))
    (when (>= (agent-run-step run) max)
      (let ((act (restart-case
                     (progn
                       (signal 'agent-max-steps
                               :run run
                               :message (format nil "max steps ~a" max))
                       :finish)
                   (continue ()
                     :report "Allow one more step and continue"
                     (setf (agent-settings-max-steps (agent-run-settings run))
                           (1+ max))
                     (progn :continue))
                   (abort ()
                     :report "Stop the run"
                     (progn :finish)))))
        (when (eq act :finish)
          (return-from %tick-run
            (%wrap-up-or-finish run callback error-callback)))))
    (incf (agent-run-step run))
    (%emit run :step (agent-run-step run))
    (%drain-inbox run)
    (%maybe-compact run
                    (lambda (did)
                      (declare (ignore did))
                      (%generate-with-instructions run callback error-callback))
                    error-callback)))

(defun %inject-instructions-or-fail (run error-callback)
  "Inject discovered instructions. T to go on; a steer error fails the run
through ERROR-CALLBACK → NIL. (STEER-INSTRUCTION-UNREADABLE offers CONTINUE /
USE-VALUE to a handler bound around the loop.)"
  (handler-case (progn (%inject-instructions run) t)
    (steer:steer-error (e)
      (funcall error-callback e)
      nil)))

(defun %generate-with-instructions (run callback error-callback)
  (when (%inject-instructions-or-fail run error-callback)
    (%do-generate run callback error-callback)))

(defmethod run-ai-agent-async ((agent ai-agent) turns &key settings tools on-event
                               on-part callback error-callback session durability)
  (%event-context)
  (let* ((settings (or (and settings (coerce-agent-settings settings))
                       (coerce-agent-settings (ai-agent-settings agent))))
         (handle (make-instance 'agent-run-handle))
         (extra (copy-list tools))
         (sid (%agent-session agent nil session))
         (dur (%coerce-durability durability))
         (run (make-agent-run
               :agent agent
               :turns (prepare-agent-turns agent turns
                                           :context (list :session sid))
               :handle handle
               :on-event on-event
               :on-part on-part
               :extra extra
               :sources (append (ai-agent-tools agent)
                                (ai-agent-handoffs agent)
                                extra)
               :settings settings
               :session sid
               :memory (ai-agent-memory agent)
               :durability dur))
         (ok (or callback (lambda (v) (declare (ignore v)))))
         (err0 (or error-callback #'error))
         (err (lambda (c)
                ;; A failed run takes its background subagents with it.
                (%cancel-subagents run)
                (funcall err0 c))))
    (setf (agent-run-parent run) *subagent-parent*)
    (%init-run-instructions run)
    (%init-journal-state run)
    (%emit run :started run)
    (%call-on-loop (lambda () (%tick-run run ok err)))
    handle))

(defmethod resume-ai-agent-async ((run agent-run) &key callback error-callback
                                  on-event on-part durability)
  (%event-context)
  (when on-event
    (setf (agent-run-on-event run) on-event))
  (when on-part
    (setf (agent-run-on-part run) on-part))
  (when durability
    (setf (agent-run-durability run) (%coerce-durability durability))
    (%init-journal-state run))
  (setf (agent-run-finish-reason run) nil)
  (let ((ok (or callback (lambda (v) (declare (ignore v)))))
        (err (or error-callback #'error))
        (ready (remove-if-not (lambda (i)
                                (eq (agent-invocation-status i) :approved))
                              (agent-run-invocations run))))
    (%call-on-loop
     (lambda ()
       (%invoke-ready run ready ok err)))
    (agent-run-handle run)))
