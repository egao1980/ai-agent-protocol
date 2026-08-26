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

(defun %backend-wake-call (backend loop fn)
  "Thread-safe enqueue. Prefer BACKEND's WAKE-CALL (libuv/libev/nio).
   DEFER + WAKE is loop-thread only — uv_idle_init is not MT-safe."
  (let* ((pkg (symbol-package (class-name (class-of backend))))
         (wake-call (and pkg (find-symbol "WAKE-CALL" pkg))))
    (if (and wake-call (fboundp wake-call))
        (funcall wake-call loop fn)
        (progn
          (event:defer backend loop fn)
          (ignore-errors (event:wake backend loop))))))

(defun %call-on-loop (fn)
  "Schedule FN on the bound loop (safe from another thread)."
  (multiple-value-bind (eb el) (%event-context)
    (%backend-wake-call eb el fn)
    nil))

(defun %off-loop (thunk callback error-callback)
  "Run THUNK on a worker; deliver the value/error on the event loop."
  (multiple-value-bind (eb el) (%event-context)
    (bt:make-thread
     (lambda ()
       (handler-case
           (let ((value (funcall thunk)))
             (event:with-event-backend (eb)
               (event:with-event-loop-var (el)
                 (%call-on-loop (lambda () (funcall callback value))))))
         (error (e)
           (event:with-event-backend (eb)
             (event:with-event-loop-var (el)
               (%call-on-loop (lambda () (funcall error-callback e))))))))
     :name "ai-agent-off-loop")
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
  (let ((fn (agent-run-on-event run)))
    (when fn
      (funcall fn kind payload))))
