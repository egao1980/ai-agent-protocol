(in-package #:ai-agent-protocol/tests)

(defun %ag-ui-input (text &key (thread "t1") (run "r1"))
  (ag-ui-protocol:make-run-agent-input
   :thread-id thread :run-id run
   :messages (list (ag-ui-protocol:make-ag-ui-message
                    :id "m1" :role "user" :content text))))

(defun %event-types (events)
  (mapcar #'ag-ui-protocol:ag-ui-event-type events))

(deftest ag-ui-handler-emits-run-events
  (with-agent-loop
    (let* ((agent (make-ai-agent :name "echo"
                                 :backend (make-mock-llm-backend)))
           (handler (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler agent))
           (events (funcall handler (%ag-ui-input "hi")))
           (types (%event-types events)))
      (ok (equal "RUN_STARTED" (first types)))
      (ok (find "TEXT_MESSAGE_CONTENT" types :test #'equal))
      (ok (equal "RUN_FINISHED" (car (last types)))))))

(deftest ag-ui-on-event-during-run
  (with-agent-loop
    (let* ((started-before-generate nil)
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (setf started-before-generate t)
                                (make-llm-response
                                 :parts (list (make-llm-text-part :text "x"))
                                 :finish-reason :stop))))
           (ai (make-ai-agent :backend backend))
           (saw-started nil)
           (handler (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler ai)))
      (let ((ag-ui-protocol:*ag-ui-emit*
              (lambda (ev)
                (when (equal "RUN_STARTED" (ag-ui-protocol:ag-ui-event-type ev))
                  (setf saw-started t)))))
        (funcall handler (%ag-ui-input "hi")))
      (ok saw-started)
      (ok started-before-generate))))

(deftest ag-ui-emits-reasoning
  ;; llm-protocol has carried thinking parts all along; the encoder used to drop
  ;; them, so reasoning never reached the UI.
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (make-llm-response
                                 :parts (list (make-llm-thinking-part
                                               :text "weighing options")
                                              (make-llm-text-part :text "done"))
                                 :finish-reason :stop))))
           (handler (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler
                     (make-ai-agent :backend backend)))
           (events (funcall handler (%ag-ui-input "hi")))
           (types (%event-types events)))
      (ok (find "REASONING_START" types :test #'equal))
      (ok (find "REASONING_MESSAGE_START" types :test #'equal))
      (ok (find "REASONING_MESSAGE_CONTENT" types :test #'equal))
      (ok (find "REASONING_MESSAGE_END" types :test #'equal))
      (ok (find "REASONING_END" types :test #'equal))
      (let ((content (find "REASONING_MESSAGE_CONTENT" events
                           :key #'ag-ui-protocol:ag-ui-event-type :test #'equal)))
        (ok (equal "weighing options"
                   (ag-ui-protocol:text-message-delta content))))
      ;; Reasoning closes before the answer it produced opens.
      (ok (< (position "REASONING_END" types :test #'equal)
             (position "TEXT_MESSAGE_START" types :test #'equal))))))

(deftest ag-ui-thinking-signature-becomes-encrypted-value
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (make-llm-response
                                 :parts (list (make-llm-thinking-part
                                               :text "hm" :signature "opaque-blob"))
                                 :finish-reason :stop))))
           (handler (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler
                     (make-ai-agent :backend backend)))
           (events (funcall handler (%ag-ui-input "hi")))
           (enc (find "REASONING_ENCRYPTED_VALUE" events
                      :key #'ag-ui-protocol:ag-ui-event-type :test #'equal)))
      (ok enc)
      (ok (equal "message" (ag-ui-protocol:reasoning-encrypted-subtype enc)))
      (ok (equal "opaque-blob" (ag-ui-protocol:reasoning-encrypted-value enc))))))

(deftest ag-ui-text-deltas
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (make-llm-response
                                 :parts (list (make-llm-text-part :text "hel")
                                              (make-llm-text-part :text "lo")
                                              (make-llm-text-part :text "!"))
                                 :finish-reason :stop))))
           (handler (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler
                     (make-ai-agent :backend backend)))
           (types (%event-types (funcall handler (%ag-ui-input "hi"))))
           (text (remove-if-not (lambda (ty) (equal ty "TEXT_MESSAGE_CONTENT")) types)))
      (ok (equal "TEXT_MESSAGE_START" (find "TEXT_MESSAGE_START" types :test #'equal)))
      (ok (= 1 (count "TEXT_MESSAGE_START" types :test #'equal)))
      (ok (= 3 (length text)))
      (ok (= 1 (count "TEXT_MESSAGE_END" types :test #'equal)))
      (ok (equal "RUN_FINISHED" (car (last types)))))))

(deftest ag-ui-tool-triad
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "sum" :arguments "{\"a\":1}"))
                               "3")))
           (agent (make-ai-agent :backend backend)))
      (define-agent-tool agent "sum" () (args)
        (declare (ignore args))
        "3")
      (let* ((events (funcall (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler agent)
                              (%ag-ui-input "1+2")))
             (types (%event-types events))
             (start (find-if (lambda (e)
                               (equal "TOOL_CALL_START"
                                      (ag-ui-protocol:ag-ui-event-type e)))
                             events))
             (args (find-if (lambda (e)
                              (equal "TOOL_CALL_ARGS"
                                     (ag-ui-protocol:ag-ui-event-type e)))
                            events))
             (end (find-if (lambda (e)
                             (equal "TOOL_CALL_END"
                                    (ag-ui-protocol:ag-ui-event-type e)))
                           events))
             (result (find-if (lambda (e)
                                (equal "TOOL_CALL_RESULT"
                                       (ag-ui-protocol:ag-ui-event-type e)))
                              events)))
        (ok (find "TOOL_CALL_START" types :test #'equal))
        (ok (find "TOOL_CALL_ARGS" types :test #'equal))
        (ok (find "TOOL_CALL_END" types :test #'equal))
        (ok (find "TOOL_CALL_RESULT" types :test #'equal))
        (ok (equal "c1" (ag-ui-protocol:tool-call-id start)))
        (ok (equal "c1" (ag-ui-protocol:tool-call-id args)))
        (ok (equal "c1" (ag-ui-protocol:tool-call-id end)))
        (ok (equal "c1" (ag-ui-protocol:tool-call-id result)))
        (ok (equal "3" (ag-ui-protocol:tool-call-result-content result)))))))

(deftest ag-ui-cancel-is-run-error
  (with-agent-loop
    (let* ((events '())
           (eb event-protocol:*event-backend*)
           (el event-protocol:*event-loop*)
           (agent (make-ai-agent :backend (make-mock-llm-backend)))
           (handle (ai-agent-protocol/ag-ui:start-ag-ui-agent-run
                    agent (%ag-ui-input "hi")
                    :on-event (lambda (ev) (push ev events))
                    :callback (lambda (run)
                                (declare (ignore run))
                                (event-protocol:stop eb el))
                    :error-callback #'error)))
      (cancel-agent-run handle)
      ;; stop-when-idle t returns before the unref'd async drains %tick-run.
      (event-protocol:run eb el :stop-when-idle nil)
      (ok (find "RUN_ERROR" (%event-types (reverse events)) :test #'equal)))))

(deftest ag-ui-unknown-part-no-crash
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (make-llm-response
                                 :parts (list (make-llm-image-part :url "http://x")
                                              (make-llm-text-part :text "ok"))
                                 :finish-reason :stop))))
           (events (funcall (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler
                             (make-ai-agent :backend backend))
                            (%ag-ui-input "hi")))
           (types (%event-types events)))
      (ok (equal "RUN_STARTED" (first types)))
      (ok (find "TEXT_MESSAGE_CONTENT" types :test #'equal))
      (ok (equal "RUN_FINISHED" (car (last types)))))))

(deftest ag-ui-passes-message-history
  (with-agent-loop
    (let* ((seen nil)
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b))
                                (setf seen (copy-list turns))
                                (make-llm-response
                                 :parts (list (make-llm-text-part :text "ok"))
                                 :finish-reason :stop))))
           (input (ag-ui-protocol:make-run-agent-input
                   :thread-id "t" :run-id "r"
                   :messages (list
                              (ag-ui-protocol:make-ag-ui-message
                               :id "1" :role "user" :content "first")
                              (ag-ui-protocol:make-ag-ui-message
                               :id "2" :role "assistant" :content "ack")
                              (ag-ui-protocol:make-ag-ui-message
                               :id "3" :role "user" :content "second"))))
           (fn (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler
                (make-ai-agent :backend backend))))
      (funcall fn input)
      (ok (equal '(:user :assistant :user)
                 (mapcar #'llm-turn-role seen)))
      (ok (equal "first" (turn-text (first seen))))
      (ok (equal "ack" (turn-text (second seen))))
      (ok (equal "second" (turn-text (third seen)))))))

(deftest ag-ui-echo-handler-unchanged
  (let* ((agent (ag-ui-protocol:make-ag-ui-agent))
         (seen '())
         (events (ag-ui-protocol:run-agent
                  agent (%ag-ui-input "ping")
                  :on-event (lambda (ev)
                              (push (ag-ui-protocol:ag-ui-event-type ev) seen)))))
    (ok (equal '("RUN_STARTED" "TEXT_MESSAGE_START" "TEXT_MESSAGE_CONTENT"
                 "TEXT_MESSAGE_END" "RUN_FINISHED")
               (mapcar #'ag-ui-protocol:ag-ui-event-type events)))
    (ok (equal (reverse seen) (mapcar #'ag-ui-protocol:ag-ui-event-type events)))))

(deftest ag-ui-run-agent-incremental
  (with-agent-loop
    (let* ((order '())
           (ai (make-ai-agent :backend (make-mock-llm-backend)))
           (agent (ag-ui-protocol:make-ag-ui-agent
                   :handler (ai-agent-protocol/ag-ui:make-ai-agent-ag-ui-handler ai)))
           (events (ag-ui-protocol:run-agent
                    agent (%ag-ui-input "hi")
                    :on-event (lambda (ev)
                                (push (ag-ui-protocol:ag-ui-event-type ev) order)))))
      (ok (equal "RUN_STARTED" (first (reverse order))))
      (ok (equal (reverse order) (mapcar #'ag-ui-protocol:ag-ui-event-type events)))
      (ok (equal "RUN_FINISHED"
                 (ag-ui-protocol:ag-ui-event-type (car (last events))))))))
