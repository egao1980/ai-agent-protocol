(in-package #:ai-agent-protocol)

;;; Default RUN / RESUME. Generate and sync tool handlers run %off-loop.

(defun %backend (run)
  (or (ai-agent-backend (agent-run-agent run))
      llm-protocol:*llm-backend*
      (error 'agent-error :message "no llm-backend on agent or *llm-backend*")))

(defun %plist-get (plist key)
  (and (consp plist) (getf plist key)))

(defun %tool-choice (run)
  "TOOL-CHOICE for this GENERATE. EXTRA :tool-choice always; :first-tool-choice
   only on step 1 (so a follow-up after a tool result is not forced)."
  (let* ((settings (agent-run-settings run))
         (extra (agent-settings-extra settings))
         (llm (agent-settings-llm settings)))
    (or (%plist-get extra :tool-choice)
        (and (= 1 (agent-run-step run))
             (%plist-get extra :first-tool-choice))
        (and (llm-settings-p llm)
             (%plist-get (llm-settings-extra llm) :tool-choice)))))

(defun %canceled-p (run)
  (agent-run-handle-canceled-p (agent-run-handle run)))

(defun %finish (run reason callback)
  (setf (agent-run-finish-reason run) reason)
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
        (%finish run :canceled callback)
        t))))

(defun %handoff-for (run name)
  (find name (ai-agent-handoffs (agent-run-agent run))
        :key #'ai-agent-name :test #'equal))

(defun %find-source (run name)
  (or (find-if (lambda (s)
                 (some (lambda (d) (equal name (llm-tool-name d)))
                       (list-agent-tools s)))
               (agent-run-sources run))
      (%handoff-for run name)))

(defun %append-invocation-turn (run inv)
  (unless (agent-invocation-recorded-p inv)
    (setf (agent-run-turns run)
          (append (agent-run-turns run)
                  (list (tool-turn (agent-invocation-id inv)
                                   (or (agent-invocation-result inv) "")
                                   :name (agent-invocation-name inv)
                                   :error-p (agent-invocation-error-p inv))))
          (agent-invocation-recorded-p inv) t)))

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
                 (agent-invocation-result inv) (format nil "unknown tool ~s" name)))))
      ((not (tool-executable-p source name))
       (setf (agent-invocation-status inv) :deferred))
      ((agent-approve-p (agent-run-agent run) source name args)
       (setf (agent-invocation-status inv) :proposed))
      (t
       (setf (agent-invocation-status inv) :approved)))
    inv))

(defun %offer-approval (run inv)
  (when (eq (agent-invocation-status inv) :proposed)
    (restart-case
        (signal 'agent-approval-required :run run :invocation inv)
      (approve ()
        :report "Approve the tool invocation"
        (setf (agent-invocation-status inv) :approved))
      (deny ()
        :report "Deny the tool invocation"
        (setf (agent-invocation-status inv) :denied
              (agent-invocation-result inv) "denied"
              (agent-invocation-error-p inv) nil))
      (pause-for-approval ()
        :report "Pause the run for HITL approval"
        nil))))

(defun %after-invocations (run callback error-callback)
  (%record-terminal-invocations run)
  (dolist (inv (agent-run-invocations run))
    (when (eq (agent-invocation-status inv) :proposed)
      (%offer-approval run inv)))
  (let ((pending (remove-if-not
                  (lambda (i)
                    (member (agent-invocation-status i)
                            '(:proposed :deferred) :test #'eq))
                  (agent-run-invocations run)))
        (ready (remove-if-not
                (lambda (i)
                  (and (eq (agent-invocation-status i) :approved)
                       (not (agent-invocation-recorded-p i))))
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
            (invoke-tool-async (agent-invocation-source inv)
                               (agent-invocation-name inv)
                               (agent-invocation-arguments inv)
                               :callback (lambda (result)
                                           (setf (agent-invocation-status inv) :done
                                                 (agent-invocation-result inv)
                                                 (if (stringp result)
                                                     result
                                                     (princ-to-string result)))
                                           (one-done))
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
                                                   (when (eq act :skip)
                                                     (setf (agent-invocation-status inv) :error
                                                           (agent-invocation-error-p inv) t
                                                           (agent-invocation-result inv)
                                                           (princ-to-string c)))
                                                   (one-done)))))))))

(defun %on-generate (run response callback error-callback)
  (handler-case
      (progn
        (when (%check-canceled run callback)
          (return-from %on-generate))
        (setf (agent-run-last-response run) response
              (agent-run-turns run)
              (append (agent-run-turns run)
                      (list (make-llm-turn
                             :role :assistant
                             :parts (copy-list (llm-response-parts response))))))
        (%emit run :response response)
        (let ((calls (llm-response-tool-calls response))
              (reason (llm-response-finish-reason response)))
          (cond
            ((null calls)
             (%finish run (or reason :stop) callback))
            (t
             (let ((invs (mapcar (lambda (c) (%classify run c)) calls)))
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
  (let ((choice (%tool-choice run)))
    (%off-loop
     (lambda ()
       (generate (%backend run)
                 (agent-run-turns run)
                 :settings (agent-settings-llm (agent-run-settings run))
                 :tools (collect-run-tools (agent-run-agent run)
                                           :extra (agent-run-extra run))
                 :tool-choice choice))
     (lambda (response)
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
                      (%on-generate run response callback error-callback)
                      :used)
                    (abort ()
                      :report "Fail the generate step"
                      (progn :fail)))))
         (cond
           ((eq act :retry)
            (%do-generate run callback error-callback))
           ((eq act :fail)
            (funcall error-callback c))))))))

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
          (return-from %tick-run (%finish run :max-steps callback)))))
    (incf (agent-run-step run))
    (%emit run :step (agent-run-step run))
    (%do-generate run callback error-callback)))

(defmethod run-ai-agent-async ((agent ai-agent) turns &key settings tools on-event
                               callback error-callback)
  (%event-context)
  (let* ((settings (or (and settings (coerce-agent-settings settings))
                       (coerce-agent-settings (ai-agent-settings agent))))
         (handle (make-instance 'agent-run-handle))
         (extra (copy-list tools))
         (run (make-agent-run
               :agent agent
               :turns (prepare-agent-turns agent turns)
               :handle handle
               :on-event on-event
               :extra extra
               :sources (append (ai-agent-tools agent)
                                (ai-agent-handoffs agent)
                                extra)
               :settings settings))
         (ok (or callback (lambda (v) (declare (ignore v)))))
         (err (or error-callback #'error)))
    (%emit run :started run)
    (%call-on-loop (lambda () (%tick-run run ok err)))
    handle))

(defmethod resume-ai-agent-async ((run agent-run) &key callback error-callback
                                  on-event)
  (%event-context)
  (when on-event
    (setf (agent-run-on-event run) on-event))
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
