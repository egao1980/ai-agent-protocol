(in-package #:ai-agent-protocol/tests)

;;; Phase-2 harness practices: retry by failure class, compaction (threshold
;;; and on overflow), the tool-less wrap-up at max steps, typed tool failures.

(defun %events-into (box)
  (lambda (kind payload)
    (push (cons kind payload) (car box))))

(defun %fast-retry ()
  (make-retry-policy :base-delay 0 :max-delay 0))

(deftest generate-retries-by-failure-class
  (with-agent-loop
    (let* ((n 0)
           (events (list nil))
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (incf n)
                                (if (< n 3)
                                    (error 'llm-http-error :status 529 :message "overloaded")
                                    (make-llm-response
                                     :parts (list (make-llm-text-part :text "back"))))))))
      (let ((run (run-ai-agent (make-ai-agent
                                :name "x" :backend backend
                                :settings (make-agent-settings
                                           :extra (list :retry-policy (%fast-retry))))
                               "hi" :on-event (%events-into events))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (equal "back" (agent-run-text run)))
        (ok (= 3 n))
        (ok (= 2 (count :retry (car events) :key #'car)))
        (ok (eq :overloaded (getf (cdr (find :retry (car events) :key #'car)) :reason)))))))

(deftest generate-does-not-retry-a-bad-request
  (with-agent-loop
    (let* ((n 0)
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (incf n)
                                (error 'llm-http-error :status 401 :message "bad key")))))
      (ok (signals (run-ai-agent (make-ai-agent
                                  :name "x" :backend backend
                                  :settings (make-agent-settings
                                             :extra (list :retry-policy (%fast-retry))))
                                 "hi")
                   'llm-http-error))
      (ok (= 1 n)))))

(defun %summarizer ()
  (make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b turns))
              (make-llm-response :parts (list (make-llm-text-part :text "SUM"))))))

(defun %history ()
  (list (user-turn "old question")
        (assistant-turn "old answer")
        (user-turn "go")))

(deftest context-overflow-compacts-once-and-retries
  (with-agent-loop
    (let* ((calls '())
           (events (list nil))
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b))
                                (push (length turns) calls)
                                (if (find-if #'conversation-protocol:summary-turn-p turns)
                                    (make-llm-response
                                     :parts (list (make-llm-text-part :text "fits now")))
                                    (error 'llm-context-overflow :status 400
                                                                 :message "prompt is too long")))))
           (mem (conversation-protocol:make-summary-memory :backend (%summarizer)
                                                           :tail-tokens 1))
           (run (run-ai-agent (make-ai-agent
                               :name "x" :backend backend
                               :settings (make-agent-settings
                                          :extra (list :compaction mem)))
                              (%history) :on-event (%events-into events))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "fits now" (agent-run-text run)))
      (ok (equal '(2 3) calls) "3 turns overflow; summary + last turn fit")
      (ok (find :compacted (car events) :key #'car))
      (ok (eq :context-overflow
              (getf (cdr (find :retry (car events) :key #'car)) :reason))))))

(deftest context-overflow-without-compactor-fails
  (with-agent-loop
    (let ((backend (make-mock-llm-backend
                    :handler (lambda (b turns &key &allow-other-keys)
                               (declare (ignore b turns))
                               (error 'llm-context-overflow :status 400)))))
      (ok (signals (run-ai-agent (make-ai-agent :name "x" :backend backend) "hi")
                   'llm-context-overflow)))))

(deftest compaction-runs-before-a-step-over-the-threshold
  (with-agent-loop
    (let* ((seen nil)
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b))
                                (setf seen turns)
                                (make-llm-response
                                 :parts (list (make-llm-text-part :text "ok"))))))
           (mem (conversation-protocol:make-summary-memory :backend (%summarizer)
                                                           :tail-tokens 1)))
      (run-ai-agent (make-ai-agent :name "x" :backend backend
                                   :settings (make-agent-settings
                                              :extra (list :compaction mem :compact-at 3)))
                    (%history))
      (ok (conversation-protocol:summary-turn-p (first seen)))
      (ok (equal "go" (turn-text (second seen))))
      (testing "under the threshold nothing is compacted"
        (run-ai-agent (make-ai-agent :name "x" :backend backend
                                     :settings (make-agent-settings
                                                :extra (list :compaction mem
                                                             :compact-at 100000)))
                      (%history))
        (ok (= 3 (length seen)))))))

(defun %loop-backend (box)
  "Calls the tool \"loop\" until tools are disabled, then summarizes."
  (make-mock-llm-backend
   :handler (lambda (b turns &key tool-choice tools &allow-other-keys)
              (declare (ignore b turns))
              (push (list tool-choice (length tools)) (car box))
              (make-llm-response
               :parts (list (if (eq tool-choice :none)
                                (make-llm-text-part :text "did 1 step, 0 left")
                                (make-llm-tool-call-part
                                 :id (format nil "c~d" (length (car box)))
                                 :name "loop" :arguments "{}")))
               :finish-reason (if (eq tool-choice :none) :stop :tool-use)))))

(deftest max-steps-wraps-up-without-tools
  (with-agent-loop
    (let* ((box (list nil))
           (agent (make-ai-agent :name "loopy" :backend (%loop-backend box)
                                 :settings (make-agent-settings :max-steps 1))))
      (define-agent-tool agent "loop" () (args)
        (declare (ignore args))
        "again")
      (let* ((run (run-ai-agent agent "go"))
             (last-turn (find :assistant (agent-run-turns run)
                              :key #'llm-turn-role :from-end t)))
        (ok (eq :max-steps (agent-run-finish-reason run)))
        (ok (= 1 (agent-run-step run)))
        (ok (equal "did 1 step, 0 left" (agent-run-text run)))
        (testing "tools stay in the request (cache prefix), tool choice is :none"
          (ok (equal '(:none 1) (first (car box))))
          (ok (= 2 (length (car box)))))
        (ok (notany #'llm-tool-call-part-p (llm-turn-parts last-turn))))
      (testing ":max-steps-wrap-up nil finishes at once"
        (setf (car box) nil)
        (let ((run (run-ai-agent agent "go"
                                 :settings (make-agent-settings
                                            :max-steps 1
                                            :extra (list :max-steps-wrap-up nil)))))
          (ok (eq :max-steps (agent-run-finish-reason run)))
          (ok (= 1 (length (car box)))))))))

(deftest typed-tool-failures-tell-the-model-what-to-do
  (with-agent-loop
    (flet ((run-tool (condition)
             (let* ((backend (make-mock-llm-backend
                              :handler (%one-shot-tools
                                        (list (make-llm-tool-call-part
                                               :id "c1" :name "q" :arguments "{}"))
                                        "ok")))
                    (agent (make-ai-agent :name "x" :backend backend)))
               (define-agent-tool agent "q" () (args)
                 (declare (ignore args))
                 (error condition))
               (agent-invocation-result
                (first (agent-run-invocations (run-ai-agent agent "go")))))))
      (let ((text (run-tool (make-condition 'tool-input-error :message "q is required"))))
        (ok (search "rejected its arguments: q is required" text))
        (ok (search "Fix the arguments" text)))
      (ok (search "Do not call it again"
                  (run-tool (make-condition 'tool-unavailable :message "no db"))))
      (ok (search "timed out" (run-tool (make-condition 'tool-timeout :message "30s"))))
      (ok (equal "Tool q failed: boom"
                 (run-tool (make-condition 'simple-error :format-control "boom"
                                                         :format-arguments nil)))))
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "lopo" :arguments "{}"))
                               "ok")))
           (agent (make-ai-agent :name "x" :backend backend)))
      (define-agent-tool agent "loop" () (args)
        (declare (ignore args))
        "x")
      (ok (equal "Unknown tool \"lopo\". Call only the tools you were given: loop."
                 (agent-invocation-result
                  (first (agent-run-invocations (run-ai-agent agent "go")))))))))
