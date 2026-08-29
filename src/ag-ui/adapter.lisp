(in-package #:ai-agent-protocol/ag-ui)

;;; Stateful encoder: agent on-event kinds → AG-UI events.
;;; Core stays AG-UI-free. Cancel (:finished :canceled) maps to RUN_ERROR.

(defclass ag-ui-encoder ()
  ((thread-id :initarg :thread-id :accessor encoder-thread-id)
   (run-id :initarg :run-id :accessor encoder-run-id)
   (open-text-id :initform nil :accessor encoder-open-text-id)
   (open-tool-ids :initform (make-hash-table :test #'equal)
                  :accessor encoder-open-tool-ids)
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
  (or (encoder-open-text-id encoder)
      (let ((id (%new-text-id encoder)))
        (setf (encoder-open-text-id encoder) id)
        (%push encoder (ag-ui-protocol:make-text-message-start-event
                        :message-id id :role "assistant"))
        id)))

(defun %tool-started-p (encoder id)
  (nth-value 1 (gethash id (encoder-open-tool-ids encoder))))

(defun %start-tool (encoder id name)
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

(defun %encode-part (encoder part)
  (cond
    ((llm-protocol:llm-text-part-p part)
     (%encode-text-part encoder part))
    ((llm-protocol:llm-thinking-part-p part)
     nil)
    ((llm-protocol:llm-tool-call-part-p part)
     (%encode-tool-part encoder part))
    (t nil)))

(defun %encode-response (encoder)
  (%close-text encoder)
  (%end-open-tools encoder))

(defun %encode-invocation (encoder inv)
  (let ((id (or (agent-invocation-id inv) "call"))
        (status (agent-invocation-status inv)))
    (case status
      (:running
       (%start-tool encoder id (agent-invocation-name inv)))
      ((:done :error :denied)
       (%push encoder (ag-ui-protocol:make-tool-call-result-event
                       :message-id (format nil "result-~a" id)
                       :tool-call-id id
                       :content (or (agent-invocation-result inv) ""))))
      (t nil))))

(defun %encode-finished (encoder run)
  (%close-text encoder)
  (%end-open-tools encoder)
  (%finish-step encoder)
  (if (and (agent-run-p run)
           (eq (agent-run-finish-reason run) :canceled))
      (%push encoder (ag-ui-protocol:make-run-error-event
                      :message "canceled" :code "canceled"))
      (%push encoder (ag-ui-protocol:make-run-finished-event
                      :thread-id (encoder-thread-id encoder)
                      :run-id (encoder-run-id encoder)))))

(defun %encode-error (encoder payload)
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

(defun ag-ui-message->turn (msg)
  "One AG-UI message → LLM-TURN (user/assistant/system/tool + toolCalls)."
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "role" h) (or (ag-ui-protocol:ag-ui-message-role msg) "user"))
    (let ((c (ag-ui-protocol:ag-ui-message-content msg)))
      (when c (setf (gethash "content" h) c)))
    (let ((name (ag-ui-protocol:ag-ui-message-name msg)))
      (when name (setf (gethash "name" h) name)))
    (let ((tid (ag-ui-protocol:ag-ui-message-tool-call-id msg)))
      (when tid (setf (gethash "tool_call_id" h) tid)))
    (let ((tcs (ag-ui-protocol:ag-ui-message-tool-calls msg)))
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
