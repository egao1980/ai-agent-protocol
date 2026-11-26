(in-package #:ai-agent-protocol/tests)

;;; #50: loading ai-agent-protocol must not change LLM-PROTOCOL's retry
;;; helpers. This system loads both, so these run in the image the bug hit.

(defun %with-auto-retry-calls (status)
  "Calls WITH-AUTO-RETRY made to a thunk that fails once with HTTP STATUS,
and whether the error escaped."
  (let ((calls 0))
    (handler-case
        (progn
          (llm-protocol:with-auto-retry
            (incf calls)
            (when (= calls 1)
              (error (llm-protocol:make-llm-http-error status))))
          (values calls nil))
      (llm-protocol:llm-http-error ()
        (values calls t)))))

(deftest with-auto-retry-skips-non-retryable-with-agent-loaded
  (dolist (status '(400 401 403))
    (multiple-value-bind (calls escaped) (%with-auto-retry-calls status)
      (ok (= 1 calls) (format nil "HTTP ~d is not retried" status))
      (ok escaped (format nil "HTTP ~d reaches the caller" status)))))

(deftest with-auto-retry-retries-retryable-with-agent-loaded
  (dolist (status '(429 503))
    (multiple-value-bind (calls escaped) (%with-auto-retry-calls status)
      (ok (= 2 calls) (format nil "HTTP ~d is retried" status))
      (ng escaped (format nil "HTTP ~d recovers" status)))))

(deftest llm-auto-retry-declines-other-conditions
  (dolist (c (list (make-condition 'simple-error :format-control "x")
                   (llm-protocol:make-llm-http-error 401)))
    (ok (eq :declined
            (restart-case
                (progn (llm-protocol:auto-retry c) :declined)
              (retry () :retried)))
        (format nil "AUTO-RETRY declines ~a" (type-of c)))))

(deftest with-auto-retry-ignores-agent-retry-restart
  ;; Before #50 the agent's AUTO-RETRY took any active RETRY restart, so an
  ;; agent restart around WITH-AUTO-RETRY re-ran a 401.
  (let ((calls 0))
    (handler-case
        (with-agent-restarts
          (llm-protocol:with-auto-retry
            (incf calls)
            (when (= calls 1)
              (error (llm-protocol:make-llm-http-error 401)))))
      (llm-protocol:llm-http-error () nil))
    (ok (= 1 calls) "the agent's RETRY restart is not used for a 401")))

(defun %generate-attempts (status)
  "(calls reason): the generate calls an agent run makes under
WITH-AUTO-RETRY-GENERATE when the first one fails with HTTP STATUS, and the
run's finish reason. The run's own retry policy is off."
  (with-agent-loop
    (let* ((n 0)
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (incf n)
                                (if (= n 1)
                                    (error (llm-protocol:make-llm-http-error status))
                                    (make-llm-response
                                     :parts (list (make-llm-text-part :text "ok"))
                                     :finish-reason :stop)))))
           (agent (make-ai-agent :name "x" :backend backend
                                 :settings (make-agent-settings
                                            :extra (list :retry-policy nil))))
           (run (handler-case (with-auto-retry-generate (run-ai-agent agent "hi"))
                  (llm-protocol:llm-http-error () nil))))
      (list n (if run (agent-run-finish-reason run) :error)))))

(deftest auto-retry-generate-retries-only-retryable-causes
  (dolist (status '(429 503))
    (destructuring-bind (n reason) (%generate-attempts status)
      (ok (= 2 n) (format nil "HTTP ~d generate is retried" status))
      (ok (eq :stop reason) (format nil "HTTP ~d run recovers" status))))
  (dolist (status '(400 401))
    (destructuring-bind (n reason) (%generate-attempts status)
      (ok (= 1 n) (format nil "HTTP ~d generate is not retried" status))
      (ng (eq :stop reason) (format nil "HTTP ~d run fails" status)))))
