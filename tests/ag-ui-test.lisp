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
           (agent (make-ai-agent :backend (make-mock-llm-backend)))
           (handle (ai-agent-protocol/ag-ui:start-ag-ui-agent-run
                    agent (%ag-ui-input "hi")
                    :on-event (lambda (ev) (push ev events)))))
      (cancel-agent-run handle)
      (event-protocol:run event-protocol:*event-backend* event-protocol:*event-loop*
                          :stop-when-idle t)
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
