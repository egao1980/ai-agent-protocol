(in-package #:ai-agent-protocol)

;;; Protocol primitive = callback + cancel token (http send-async shape).
;;; Promises stay in a later facade. Sync = await on event-protocol.

(defun %event-context ()
  (let ((eb event:*event-backend*)
        (el event:*event-loop*))
    (unless (and eb el)
      (error 'agent-missing-loop
             :message "bind event-protocol:*event-backend* and *event-loop*"))
    (values eb el)))

(defun %call-on-loop (fn)
  "Schedule FN on the bound loop (safe from another thread)."
  (multiple-value-bind (eb el) (%event-context)
    (event:wake-call eb el fn)
    nil))

(defun %off-loop (thunk callback error-callback)
  "Run THUNK on a worker; deliver the value/error on the event loop."
  (multiple-value-bind (eb el) (%event-context)
    (event:submit eb el thunk
                  :callback callback
                  :error-callback error-callback)
    nil))

(defun %await (start-fn &key timeout)
  "Drive the bound loop until START-FN's callback/error-callback fires."
  (multiple-value-bind (eb el) (%event-context)
    (let ((result nil)
          (err nil)
          (done nil))
      (funcall start-fn
               (lambda (value)
                 (setf result value done t)
                 (event:stop eb el))
               (lambda (c)
                 (setf err c done t)
                 (event:stop eb el)))
      (when timeout
        (event:sleep* eb el timeout
                      :callback (lambda ()
                                  (unless done
                                    (setf err (make-condition
                                               'agent-timeout
                                               :message "agent await timed out")
                                          done t)
                                    (event:stop eb el)))))
      (event:run eb el :stop-when-idle nil)
      (when err (error err))
      result)))

(defun %emit (run kind payload)
  (when (and (eq kind :part) (agent-run-on-part run))
    (funcall (agent-run-on-part run) payload))
  (when (agent-run-on-event run)
    (funcall (agent-run-on-event run) kind payload)))
