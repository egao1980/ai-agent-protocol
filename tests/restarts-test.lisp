(in-package #:ai-agent-protocol/tests)

(deftest unknown-tool-skip-continues
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "nope" :arguments "{}"))
                               "survived")))
           (agent (make-ai-agent :name "x" :backend backend))
           (run (run-ai-agent agent "go")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "survived" (agent-run-text run)))
      (ok (eq :error (agent-invocation-status (first (agent-run-invocations run))))))))

(deftest unknown-tool-use-value
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "ghost" :arguments "{}"))
                               "after")))
           (agent (make-ai-agent :name "x" :backend backend))
           (run (handler-bind ((agent-unknown-tool
                                (lambda (c)
                                  (use-value "from-restart" c))))
                  (run-ai-agent agent "go"))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (eq :done (agent-invocation-status (first (agent-run-invocations run)))))
      (ok (equal "from-restart"
                 (agent-invocation-result (first (agent-run-invocations run))))))))

(deftest tool-error-use-value
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "boom" :arguments "{}"))
                               "ok")))
           (agent (make-ai-agent :name "x" :backend backend)))
      (define-agent-tool agent "boom" () (args)
        (declare (ignore args))
        (error "exploded"))
      (let ((run (handler-bind ((agent-tool-error
                                 (lambda (c)
                                   (use-value "patched" c))))
                   (run-ai-agent agent "go"))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (eq :done (agent-invocation-status (first (agent-run-invocations run)))))
        (ok (equal "patched"
                   (agent-invocation-result (first (agent-run-invocations run)))))))))

(deftest generate-retry
  (with-agent-loop
    (let* ((n 0)
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (incf n)
                                (if (= n 1)
                                    (error 'llm-protocol:llm-http-error
                                           :status 429 :message "rate" :retryable-p t)
                                    (make-llm-response
                                     :parts (list (make-llm-text-part :text "recovered"))
                                     :finish-reason :stop))))))
      (let* ((agent (make-ai-agent :name "x" :backend backend))
             (run (handler-bind ((agent-generate-error
                                  (lambda (c)
                                    (invoke-retry c))))
                    (run-ai-agent agent "hi"))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (equal "recovered" (agent-run-text run)))
        (ok (= 2 n))))))

(deftest max-steps-continue
  (with-agent-loop
    (let* ((n 0)
           (backend (make-mock-llm-backend
                     :tool-calls (list (make-llm-tool-call-part
                                        :id "c1" :name "loop" :arguments "{}"))))
           (agent (make-ai-agent
                   :name "loopy" :backend backend
                   :settings (make-agent-settings :max-steps 1))))
      (define-agent-tool agent "loop" () (args)
        (declare (ignore args))
        "again")
      (let ((run (handler-bind ((agent-max-steps
                                 (lambda (c)
                                   (incf n)
                                   (when (= n 1)
                                     (continue c)))))
                    (run-ai-agent agent "go"))))
        (ok (eq :max-steps (agent-run-finish-reason run)))
        (ok (= 2 n))
        (ok (= 2 (agent-run-step run)))))))

(deftest cancel-signals-agent-canceled
  (with-agent-loop
    (let* ((agent (make-ai-agent :backend (make-mock-llm-backend)))
           (seen nil)
           (run nil)
           (handle (handler-bind ((agent-canceled
                                   (lambda (c)
                                     (setf seen c)
                                     nil)))
                     (run-ai-agent-async
                      agent "hi"
                      :callback (lambda (r) (setf run r))
                      :error-callback #'error))))
      (cancel-agent-run handle)
      (event-protocol:run event-protocol:*event-backend* event-protocol:*event-loop*
                          :stop-when-idle t)
      (ok (typep seen 'agent-canceled))
      (ok (or (null run)
              (eq :canceled (agent-run-finish-reason run)))))))

(deftest approval-default-still-pauses
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "danger" :arguments "{}"))
                               "ok")))
           (agent (make-ai-agent :name "gated" :backend backend)))
      (register-agent-tool
       agent
       (make-function-tool :name "danger" :approval-required-p t
                           :handler (lambda (args)
                                      (declare (ignore args))
                                      "did-it")))
      (let ((run (run-ai-agent agent "go")))
        (ok (eq :approval (agent-run-finish-reason run)))
        (ok (= 1 (length (agent-run-pending run))))))))

(deftest approval-approve-restart
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "danger" :arguments "{}"))
                               "ok")))
           (agent (make-ai-agent :name "gated" :backend backend)))
      (register-agent-tool
       agent
       (make-function-tool :name "danger" :approval-required-p t
                           :handler (lambda (args)
                                      (declare (ignore args))
                                      "did-it")))
      (let ((run (handler-bind ((agent-approval-required
                                 (lambda (c)
                                   (invoke-approve c))))
                   (run-ai-agent agent "go"))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (equal "did-it"
                   (agent-invocation-result (first (agent-run-invocations run)))))))))
