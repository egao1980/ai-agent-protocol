(in-package #:ai-agent-protocol/ag-ui)

;;; Stateful encoder: agent on-event kinds → AG-UI events.
;;; Core stays AG-UI-free. Cancel (:finished :canceled) maps to RUN_ERROR.

(defclass ag-ui-encoder ()
  ((thread-id :initarg :thread-id :accessor encoder-thread-id)
   (run-id :initarg :run-id :accessor encoder-run-id)
   (open-text-id :initform nil :accessor encoder-open-text-id)
   (open-reasoning-id :initform nil :accessor encoder-open-reasoning-id)
   (open-tool-ids :initform (make-hash-table :test #'equal)
                  :accessor encoder-open-tool-ids)
   ;; Tool calls announced by an earlier run and now being resumed. The spec
   ;; says a resumed run reports the result against the original toolCallId
   ;; without re-emitting Start/Args/End, so these are suppressed.
   (resumed-tool-ids :initform (make-hash-table :test #'equal)
                     :accessor encoder-resumed-tool-ids)
   (current-step :initform nil :accessor encoder-current-step)
   (on-event :initarg :on-event :initform nil :accessor encoder-on-event)
   (events :initform nil :accessor encoder-events)))

(defun make-ag-ui-encoder (&key thread-id run-id on-event)
  (make-instance 'ag-ui-encoder
                 :thread-id (or thread-id "thread")
                 :run-id (or run-id "run")
                 :on-event on-event))

(defun ag-ui-encoder-events (encoder)
  (copy-list (encoder-events encoder)))

(defun %push (encoder event)
  (setf (encoder-events encoder)
        (nconc (encoder-events encoder) (list event)))
  (ag-ui-protocol:ag-ui-emit event)
  (when (encoder-on-event encoder)
    (funcall (encoder-on-event encoder) event))
  event)

(defun %new-text-id (encoder)
  (format nil "msg-~a-~a" (encoder-run-id encoder)
          (length (encoder-events encoder))))

(defun %close-text (encoder)
  (let ((id (encoder-open-text-id encoder)))
    (when id
      (%push encoder (ag-ui-protocol:make-text-message-end-event :message-id id))
      (setf (encoder-open-text-id encoder) nil))))

(defun %close-reasoning (encoder)
  (let ((id (encoder-open-reasoning-id encoder)))
    (when id
      (%push encoder (ag-ui-protocol:make-reasoning-message-end-event
                      :message-id id))
      (%push encoder (ag-ui-protocol:make-reasoning-end-event :message-id id))
      (setf (encoder-open-reasoning-id encoder) nil))))

(defun %end-tool (encoder id)
  (when (gethash id (encoder-open-tool-ids encoder))
    (%push encoder (ag-ui-protocol:make-tool-call-end-event :tool-call-id id))
    (remhash id (encoder-open-tool-ids encoder))))

(defun %end-open-tools (encoder)
  (let ((ids '()))
    (maphash (lambda (id ignored)
               (declare (ignore ignored))
               (push id ids))
             (encoder-open-tool-ids encoder))
    (dolist (id (nreverse ids))
      (%end-tool encoder id))))

(defun %finish-step (encoder)
  (let ((name (encoder-current-step encoder)))
    (when name
      (%push encoder (ag-ui-protocol:make-step-finished-event :step-name name))
      (setf (encoder-current-step encoder) nil))))

(defun %start-step (encoder name)
  (%finish-step encoder)
  (setf (encoder-current-step encoder) name)
  (%push encoder (ag-ui-protocol:make-step-started-event :step-name name)))

(defun %ensure-text (encoder)
  ;; Reasoning precedes the answer it produced, so opening text closes it.
  (%close-reasoning encoder)
  (or (encoder-open-text-id encoder)
      (let ((id (%new-text-id encoder)))
        (setf (encoder-open-text-id encoder) id)
        (%push encoder (ag-ui-protocol:make-text-message-start-event
                        :message-id id :role "assistant"))
        id)))

(defun %ensure-reasoning (encoder)
  (or (encoder-open-reasoning-id encoder)
      (let ((id (format nil "reasoning-~a-~a" (encoder-run-id encoder)
                        (length (encoder-events encoder)))))
        (%close-text encoder)
        (setf (encoder-open-reasoning-id encoder) id)
        (%push encoder (ag-ui-protocol:make-reasoning-start-event :message-id id))
        (%push encoder (ag-ui-protocol:make-reasoning-message-start-event
                        :message-id id))
        id)))

(defun %tool-started-p (encoder id)
  (nth-value 1 (gethash id (encoder-open-tool-ids encoder))))

(defun mark-tool-resumed (encoder id)
  "Suppress the tool-call triad for ID; only its result belongs in this run."
  (setf (gethash id (encoder-resumed-tool-ids encoder)) t))

(defun %start-tool (encoder id name)
  (%close-reasoning encoder)
  (when (gethash id (encoder-resumed-tool-ids encoder))
    (return-from %start-tool nil))
  (unless (%tool-started-p encoder id)
    (setf (gethash id (encoder-open-tool-ids encoder)) t)
    (%push encoder (ag-ui-protocol:make-tool-call-start-event
                    :tool-call-id id
                    :tool-call-name (or name "")))))

(defun %encode-text-part (encoder part)
  (let ((id (%ensure-text encoder))
        (delta (or (llm-protocol:llm-text-part-text part) "")))
    (%push encoder (ag-ui-protocol:make-text-message-content-event
                    :message-id id :delta delta))))

(defun %encode-tool-part (encoder part)
  (let ((id (or (llm-protocol:llm-tool-call-part-id part) "call"))
        (name (llm-protocol:llm-tool-call-part-name part))
        (args (or (llm-protocol:llm-tool-call-part-arguments part) "")))
    (%start-tool encoder id name)
    (%push encoder (ag-ui-protocol:make-tool-call-args-event
                    :tool-call-id id :delta args))))

(defun %encode-thinking-part (encoder part)
  (let ((id (%ensure-reasoning encoder))
        (text (or (llm-protocol:llm-thinking-part-text part) ""))
        (signature (llm-protocol:llm-thinking-part-signature part)))
    (when (plusp (length text))
      (%push encoder (ag-ui-protocol:make-reasoning-message-content-event
                      :message-id id :delta text)))
    ;; A provider signature is opaque chain-of-thought the client stores and
    ;; echoes back untouched — the encrypted-reasoning carry-over the spec
    ;; describes for zero-data-retention providers.
    (when signature
      (%push encoder (ag-ui-protocol:make-reasoning-encrypted-value-event
                      :subtype "message" :entity-id id
                      :encrypted-value signature)))))

(defun %encode-part (encoder part)
  (cond
    ((llm-protocol:llm-text-part-p part)
     (%encode-text-part encoder part))
    ((llm-protocol:llm-thinking-part-p part)
     (%encode-thinking-part encoder part))
    ((llm-protocol:llm-tool-call-part-p part)
     (%encode-tool-part encoder part))
    (t nil)))

(defun %encode-response (encoder)
  (%close-reasoning encoder)
  (%close-text encoder)
  (%end-open-tools encoder))

(defun %encode-invocation (encoder inv)
  (let ((id (or (agent-invocation-id inv) "call"))
        (status (agent-invocation-status inv)))
    (case status
      ;; A proposed invocation is the agent asking permission. It still gets the
      ;; tool-call triad so the UI can show what is being requested; the ask
      ;; itself rides out on the interrupt outcome at RUN_FINISHED.
      (:proposed
       (%start-tool encoder id (agent-invocation-name inv))
       (%push encoder (ag-ui-protocol:make-tool-call-args-event
                       :tool-call-id id
                       :delta (or (agent-invocation-arguments inv) "{}")))
       (%end-tool encoder id))
      (:running
       (%start-tool encoder id (agent-invocation-name inv)))
      ((:done :error :denied)
       (%push encoder (ag-ui-protocol:make-tool-call-result-event
                       :message-id (format nil "result-~a" id)
                       :tool-call-id id
                       :content (or (agent-invocation-result inv) ""))))
      (t nil))))

(defparameter +approval-response-schema-json+
  "{\"type\":\"object\",\"properties\":{\"approved\":{\"type\":\"boolean\"},\"editedArgs\":{\"type\":\"object\",\"description\":\"Full replacement of the tool args. Not merged.\"},\"always\":{\"type\":\"boolean\",\"description\":\"With approved: also allow matching calls of this tool for the rest of the run, and save the rule to the agent's permission store.\"},\"rejectAll\":{\"type\":\"boolean\",\"description\":\"Reject this call and every other open tool-call interrupt of the run. The other interrupts may then be left out of the resume.\"},\"feedback\":{\"type\":\"string\",\"description\":\"With a rejection: tell the model what to do instead.\"}},\"required\":[\"approved\"]}"
  "Response schema offered for tool-call interrupts. Each optional property
   advertises a reply a client may offer: editedArgs approve-with-edits, always
   approve-always (APPROVE-INVOCATION :always), rejectAll reject-all-pending
   (DENY-INVOCATION :all), feedback a rejection message for the model.")

(defparameter +approval-replies+ '("once" "always" "reject" "rejectAll")
  "The approval replies a tool-call interrupt offers, in its metadata
   (`replies`), named after opencode's once / always / reject.")

(defun %approval-response-schema ()
  (ag-ui-protocol:decode-json +approval-response-schema-json+))

(defun invocation-interrupt (inv)
  "One pending invocation as an AG-UI interrupt."
  (let ((id (or (agent-invocation-id inv) "call")))
    (ag-ui-protocol:make-interrupt
     :id (format nil "int-~a" id)
     :reason "tool_call"
     :tool-call-id id
     :message (format nil "Approve ~a?" (agent-invocation-name inv))
     :response-schema (%approval-response-schema)
     :metadata (ag-ui-protocol:json-object
                "replies" (coerce +approval-replies+ 'vector)))))

(defun run-interrupts (run)
  "Interrupts for every invocation RUN is waiting on."
  (when (agent-run-p run)
    (mapcar #'invocation-interrupt
            (remove-if-not (lambda (inv)
                             (eq (agent-invocation-status inv) :proposed))
                           (agent-run-pending run)))))

(defun %encode-finished (encoder run)
  (%close-reasoning encoder)
  (%close-text encoder)
  (%end-open-tools encoder)
  (%finish-step encoder)
  (let ((interrupts (run-interrupts run)))
    (cond
      ((and (agent-run-p run) (eq (agent-run-finish-reason run) :canceled))
       (%push encoder (ag-ui-protocol:make-run-error-event
                       :message "canceled" :code "canceled")))
      ;; Paused for approval: end the run with an interrupt outcome so the
      ;; client knows to ask, rather than finishing as if nothing were pending.
      (interrupts
       (%push encoder (ag-ui-protocol:make-run-interrupted-event
                       :thread-id (encoder-thread-id encoder)
                       :run-id (encoder-run-id encoder)
                       :interrupts interrupts)))
      (t
       (%push encoder (ag-ui-protocol:make-run-finished-event
                       :thread-id (encoder-thread-id encoder)
                       :run-id (encoder-run-id encoder)))))))

(defun %encode-error (encoder payload)
  (%close-reasoning encoder)
  (%close-text encoder)
  (%end-open-tools encoder)
  (%finish-step encoder)
  (%push encoder (ag-ui-protocol:make-run-error-event
                  :message (princ-to-string payload))))

(defun encode-agent-event (encoder kind payload)
  "Map one agent on-event (KIND PAYLOAD) to zero or more AG-UI events."
  (ecase kind
    (:started
     (%push encoder (ag-ui-protocol:make-run-started-event
                     :thread-id (encoder-thread-id encoder)
                     :run-id (encoder-run-id encoder))))
    (:step
     (%start-step encoder (format nil "step-~a" payload)))
    (:part
     (%encode-part encoder payload))
    (:response
     (%encode-response encoder))
    (:invocation
     (%encode-invocation encoder payload))
    (:handoff
     (let ((name (if (ai-agent-p payload)
                     (ai-agent-name payload)
                     (princ-to-string payload))))
       (%start-step encoder name)
       (%finish-step encoder)))
    (:finished
     (%encode-finished encoder payload))
    (:error
     (%encode-error encoder payload)))
  encoder)

(defun %input-ids (input)
  (values (or (ag-ui-protocol:run-agent-input-thread-id input) "thread")
          (or (ag-ui-protocol:run-agent-input-run-id input) "run")))

(defun %slot (fn object)
  (handler-case (funcall fn object)
    (unbound-slot () nil)))

(defun ag-ui-message->turn (msg)
  "One AG-UI message → LLM-TURN (user/assistant/system/tool + toolCalls)."
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "role" h)
          (or (%slot #'ag-ui-protocol:ag-ui-message-role msg) "user"))
    (let ((c (%slot #'ag-ui-protocol:ag-ui-message-content msg)))
      (when c (setf (gethash "content" h) c)))
    (let ((name (%slot #'ag-ui-protocol:ag-ui-message-name msg)))
      (when name (setf (gethash "name" h) name)))
    (let ((tid (%slot #'ag-ui-protocol:ag-ui-message-tool-call-id msg)))
      (when tid (setf (gethash "tool_call_id" h) tid)))
    (let ((tcs (%slot #'ag-ui-protocol:ag-ui-message-tool-calls msg)))
      (when tcs (setf (gethash "toolCalls" h) tcs)))
    (llm-protocol:coerce-turn h)))

(defun ag-ui-input-turns (input)
  "Full `run-agent-input` messages as LLM turns. Empty → last-user-text fallback."
  (let ((msgs (ag-ui-protocol:run-agent-input-messages input)))
    (if (and msgs (plusp (length msgs)))
        (map 'list #'ag-ui-message->turn msgs)
        (list (llm-protocol:user-turn (ag-ui-protocol:last-user-text input))))))

(defun start-ag-ui-agent-run (ai-agent input &key settings on-event
                              callback error-callback)
  "Run AI-AGENT asynchronously, encoding AG-UI events onto ON-EVENT.
   Passes the full input.messages thread (not only last user text).
   Returns an AGENT-RUN-HANDLE (cancel token)."
  (multiple-value-bind (thread run-id)
      (%input-ids input)
    (let ((encoder (make-ag-ui-encoder :thread-id thread :run-id run-id
                                       :on-event on-event)))
      (run-ai-agent-async
       ai-agent (ag-ui-input-turns input)
       :settings settings
       :on-event (lambda (kind payload)
                   (encode-agent-event encoder kind payload))
       :callback callback
       :error-callback (lambda (c)
                         (encode-agent-event encoder :error c)
                         (when error-callback (funcall error-callback c)))))))

(defun %invocation-for-interrupt (run interrupt-id)
  (find-if (lambda (inv)
             (equal (format nil "int-~a" (agent-invocation-id inv)) interrupt-id))
           (agent-run-pending run)))

(defun %payload-field (entry key)
  "KEY of a resolved ENTRY's payload object, or NIL."
  (let ((payload (ag-ui-protocol:event-field entry 'ag-ui-protocol::payload)))
    (and (equal (ag-ui-protocol:resume-status entry) "resolved")
         (hash-table-p payload)
         (gethash key payload))))

(defun %resume-always-p (entry)
  (and (ag-ui-protocol:resume-approved-p entry)
       (%payload-field entry "always")))

(defun %resume-reject-all-p (entry)
  (and (%payload-field entry "rejectAll") t))

(defun %resume-feedback (entry)
  (let ((feedback (%payload-field entry "feedback")))
    (and (stringp feedback) (plusp (length feedback)) feedback)))

(defun %deny-from-entry (run inv entry &key all)
  (deny-invocation
   run inv
   :reason (if (equal (ag-ui-protocol:resume-status entry) "cancelled")
               "cancelled"
               "denied")
   :feedback (%resume-feedback entry)
   :all all))

(defun apply-resume (run input)
  "Apply INPUT's resume entries to RUN's pending invocations.

   Validates against the interrupts RUN actually has open first, so a resume
   that violates the contract signals AG-UI-RESUME-ERROR rather than half
   applying. Returns the number of invocations decided.

   Payload replies (advertised by INVOCATION-INTERRUPT's responseSchema):
   `approved` approves once; with `always` it is APPROVE-INVOCATION :always
   (matching calls are allowed for the rest of the run and the rule is saved to
   the permission store); `feedback` goes to the model with a rejection.
   `rejectAll` on any entry is DENY-INVOCATION :all: every open tool-call
   interrupt is rejected with that entry's feedback, whatever the other entries
   say, and the others may be left out of the resume. Always replies are
   applied after the other entries, so an explicit answer for a call is never
   overridden by another call's always rule."
  (let* ((interrupts (run-interrupts run))
         (entries (coerce (or (ag-ui-protocol:event-field input 'ag-ui-protocol::resume)
                              #())
                          'list))
         (reject-all (find-if #'%resume-reject-all-p entries))
         (decided 0))
    (flet ((inv-of (entry)
             (%invocation-for-interrupt run (ag-ui-protocol:resume-interrupt-id entry))))
      (cond
        (reject-all
         ;; Only the answered interrupts are checked: reject-all answers the rest.
         (ag-ui-protocol:validate-resume
          (remove-if-not (lambda (int)
                           (find (ag-ui-protocol:interrupt-id int) entries
                                 :key #'ag-ui-protocol:resume-interrupt-id
                                 :test #'equal))
                         interrupts)
          entries)
         (let ((inv (inv-of reject-all)))
           (when inv
             (setf decided (length interrupts))
             (%deny-from-entry run inv reject-all :all t))))
        (t
         (ag-ui-protocol:validate-resume interrupts entries)
         (dolist (entry (append (remove-if #'%resume-always-p entries)
                                (remove-if-not #'%resume-always-p entries)))
           (let ((inv (inv-of entry)))
             (when inv
               (incf decided)
               (if (ag-ui-protocol:resume-approved-p entry)
                   (let ((edited (ag-ui-protocol:resume-edited-args entry)))
                     ;; editedArgs fully replaces the proposed arguments.
                     (when edited
                       (setf (agent-invocation-arguments inv)
                             (ag-ui-protocol:encode-json edited)))
                     (approve-invocation run inv :always (%resume-always-p entry)))
                   (%deny-from-entry run inv entry))))))))
    decided))

(defun resume-ag-ui-agent-run (run input &key on-event)
  "Answer RUN's open interrupts from INPUT and continue it, encoding AG-UI
   events onto ON-EVENT. The resumed run re-uses the original tool call ids, so
   it emits TOOL_CALL_RESULT without re-proposing the call."
  (multiple-value-bind (thread run-id) (%input-ids input)
    (let ((encoder (make-ag-ui-encoder :thread-id thread :run-id run-id
                                       :on-event on-event)))
      ;; Note the ids before resuming: once the run continues they leave
      ;; `pending` and there is no way to tell them from fresh calls.
      (dolist (inv (agent-run-pending run))
        (mark-tool-resumed encoder (agent-invocation-id inv)))
      (apply-resume run input)
      (handler-case
          (resume-ai-agent run
                           :on-event (lambda (kind payload)
                                       (encode-agent-event encoder kind payload)))
        (error (c) (encode-agent-event encoder :error c)))
      (ag-ui-encoder-events encoder))))

(defun make-ai-agent-ag-ui-handler (agent &key settings)
  "Sync AG-UI handler. Emits via AG-UI-EMIT as events are encoded while awaiting."
  (lambda (input)
    (multiple-value-bind (thread run-id)
        (%input-ids input)
      (let ((encoder (make-ag-ui-encoder :thread-id thread :run-id run-id)))
        (handler-case
            (run-ai-agent agent (ag-ui-input-turns input)
                          :settings settings
                          :on-event (lambda (kind payload)
                                      (encode-agent-event encoder kind payload)))
          (error (c)
            (encode-agent-event encoder :error c)))
        (ag-ui-encoder-events encoder)))))
