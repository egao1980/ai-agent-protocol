(in-package #:ai-agent-protocol/tests)

;;; 0.16.0 resilience (src/resilience.lisp): cancel during a backoff, richer
;;; :retry / :compacted events, output continuation, truncated tool calls,
;;; manual compaction, transport errors retried, no re-run of tools.

(defun %scripted (&rest responses)
  "A handler answering each generate with the next of RESPONSES; a condition
designator (a function) is called instead (it signals). Records LLM settings."
  (let ((n 0) (seen (list nil)))
    (values
     (lambda (b turns &key settings &allow-other-keys)
       (declare (ignore b turns))
       (push settings (car seen))
       (let ((r (nth n responses)))
         (incf n)
         (if (functionp r) (funcall r) r)))
     seen)))

(defun %resp (text &key (finish :stop) calls)
  (make-llm-response :parts (append (and text (list (make-llm-text-part :text text)))
                                    calls)
                     :finish-reason finish))

(deftest transport-error-is-retried
  (with-agent-loop
    (let* ((events (list nil))
           (agent (make-ai-agent
                   :name "x"
                   :backend (make-mock-llm-backend
                             :handler (%scripted
                                       (lambda () (error 'llm-transport-error :kind :reset))
                                       (%resp "back")))
                   :settings (make-agent-settings :timeout 20
                              :extra (list :retry-policy (make-retry-policy :base-delay 0
                                                                            :max-delay 0)))))
           (run (run-ai-agent agent "hi" :on-event (%events-into events))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "back" (agent-run-text run)))
      (let ((retry (cdr (find :retry (car events) :key #'car))))
        (ok (eq :transient (getf retry :reason)))
        (ok (eql 1 (getf retry :attempt)))
        (ok (eql 5 (getf retry :max-attempts)))
        (ok (member :status retry))))))

(deftest retry-event-carries-status-and-retry-after
  (with-agent-loop
    (let* ((events (list nil))
           (agent (make-ai-agent
                   :name "x"
                   :backend (make-mock-llm-backend
                             :handler (%scripted
                                       (lambda () (error 'llm-rate-limited :status 429
                                                                           :retry-after 0))
                                       (%resp "ok")))
                   :settings (make-agent-settings :timeout 20
                              :extra (list :retry-policy (make-retry-policy :max-attempts 3)))))
           (run (run-ai-agent agent "hi" :on-event (%events-into events))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (let ((retry (cdr (find :retry (car events) :key #'car))))
        (ok (eq :rate-limit (getf retry :reason)))
        (ok (eql 429 (getf retry :status)))
        (ok (eql 0 (getf retry :retry-after)))
        (ok (eql 3 (getf retry :max-attempts)))))))

(deftest cancel-during-backoff-is-prompt
  (with-agent-loop
    (let* ((run-box (list nil))
           (calls 0)
           (agent (make-ai-agent
                   :name "x"
                   :backend (make-mock-llm-backend
                             :handler (lambda (b turns &key &allow-other-keys)
                                        (declare (ignore b turns))
                                        (incf calls)
                                        (error 'llm-overloaded :status 529)))
                   :settings (make-agent-settings :timeout 20
                              :extra (list :retry-policy (make-retry-policy :base-delay 30
                                                                            :max-delay 30)))))
           (start (get-internal-real-time))
           (run (run-ai-agent agent "hi"
                              :on-event (lambda (kind payload)
                                          (case kind
                                            (:started (setf (car run-box) payload))
                                            (:retry (cancel-agent-run (car run-box))))))))
      (ok (eq :canceled (agent-run-finish-reason run)))
      (ok (= 1 calls) "no generate after the cancel")
      (ok (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 5)
          "the 30 s backoff did not run out"))))

(deftest mid-stream-failure-after-a-tool-does-not-rerun-it
  (with-agent-loop
    (let* ((ran 0)
           (agent (make-ai-agent
                   :name "x"
                   :backend (make-mock-llm-backend
                             :handler (%scripted
                                       (%resp "" :finish :tool-use
                                                 :calls (list (make-llm-tool-call-part
                                                               :id "c1" :name "touch"
                                                               :arguments "{}")))
                                       (lambda () (error 'llm-incomplete-response
                                                         :partial (%resp "half" :finish :incomplete)))
                                       (%resp "done")))
                   :settings (make-agent-settings :timeout 20
                              :extra (list :retry-policy (make-retry-policy :base-delay 0
                                                                            :max-delay 0))))))
      (define-agent-tool agent "touch" (:description "side effect") (args)
        (declare (ignore args))
        (incf ran)
        "touched")
      (let ((run (run-ai-agent agent "go")))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (= 1 ran) "the tool ran once")
        (ok (equal "done" (agent-run-text run)))
        (ok (notany (lambda (turn)
                      (search "half" (or (turn-text turn) "")))
                    (agent-run-turns run))
            "the partial text was dropped")))))

(deftest truncated-calls-of-a-response
  (let ((a (make-llm-tool-call-part :id "a" :name "write" :arguments "{\"p\":1}"))
        (b (make-llm-tool-call-part :id "b" :name "write" :arguments "{\"content\":\"abc")))
    (ok (equal (list b) (response-truncated-calls (%resp nil :finish :length :calls (list a b)))))
    (ok (null (response-truncated-calls (%resp nil :finish :tool-use :calls (list a b)))))
    (ok (null (response-truncated-calls (%resp "text" :finish :length))))))

(deftest continuation-turns
  (let ((turn (continuation-turn)))
    (ok (eq :user (llm-turn-role turn)))
    (ok (equal +continuation-prompt+ (turn-text turn)))
    (ok (continuation-turn-p turn))
    (ok (continuation-turn-p (continuation-turn "go on") "go on"))
    (ng (continuation-turn-p (user-turn "hello")))
    (ng (continuation-turn-p (assistant-turn +continuation-prompt+)))))

(deftest length-continues-up-to-the-limit
  (with-agent-loop
    (let* ((events (list nil))
           (agent (make-ai-agent
                   :name "x"
                   :backend (make-mock-llm-backend
                             :handler (%scripted (%resp "one " :finish :length)
                                                 (%resp "two " :finish :length)
                                                 (%resp "three" :finish :stop)))
                   :settings (make-agent-settings :timeout 20 :extra (list :max-continuations 2))))
           (run (run-ai-agent agent "write" :on-event (%events-into events))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "one two three" (agent-run-text run)))
      (ok (= 2 (count :continuation (car events) :key #'car)))
      (ok (= 2 (count-if #'continuation-turn-p (agent-run-turns run))))
      (let ((c (cdr (find :continuation (car events) :key #'car))))
        (ok (eql 2 (getf c :max)))))))

(deftest length-past-the-limit-finishes-length
  (with-agent-loop
    (let* ((agent (make-ai-agent
                   :name "x"
                   :backend (make-mock-llm-backend
                             :handler (%scripted (%resp "a" :finish :length)
                                                 (%resp "b" :finish :length)))
                   :settings (make-agent-settings :timeout 20 :extra (list :max-continuations 1))))
           (run (run-ai-agent agent "write")))
      (ok (eq :length (agent-run-finish-reason run)))
      (ok (equal "ab" (agent-run-text run))))))

(deftest length-without-continuations-is-unchanged
  (with-agent-loop
    (let* ((n 0)
           (agent (make-ai-agent
                   :name "x"
                   :backend (make-mock-llm-backend
                             :handler (lambda (b turns &key &allow-other-keys)
                                        (declare (ignore b turns))
                                        (incf n)
                                        (%resp "cut" :finish :length)))))
           (run (run-ai-agent agent "write")))
      (ok (eq :length (agent-run-finish-reason run)))
      (ok (= 1 n)))))

(defun %write-agent (handler &rest extra)
  (let ((agent (make-ai-agent :name "x"
                              :backend (make-mock-llm-backend :handler handler)
                              :settings (make-agent-settings :timeout 20
                                         :llm (make-llm-settings :max-tokens 100)
                                         :extra extra))))
    agent))

(deftest truncated-tool-call-retried-with-more-tokens
  (with-agent-loop
    (multiple-value-bind (handler seen)
        (%scripted (%resp "" :finish :length
                             :calls (list (make-llm-tool-call-part
                                           :id "c1" :name "write" :arguments "{\"x\":\"aa")))
                   (%resp "" :finish :tool-use
                             :calls (list (make-llm-tool-call-part
                                           :id "c2" :name "write" :arguments "{\"x\":\"aaaa\"}")))
                   (%resp "written"))
      (let* ((events (list nil))
             (got '())
             (agent (%write-agent handler :length-retry-max-tokens 400)))
        (define-agent-tool agent "write" (:description "write") (args)
          (push args got)
          "ok")
        (let ((run (run-ai-agent agent "go" :on-event (%events-into events))))
          (ok (eq :stop (agent-run-finish-reason run)))
          (ok (= 1 (length got)) "only the complete call ran")
          (let ((settings (reverse (car seen))))
            (ok (eql 100 (llm-settings-max-tokens (coerce-settings (first settings)))))
            (ok (eql 400 (llm-settings-max-tokens (coerce-settings (second settings)))))
            (ok (eql 100 (llm-settings-max-tokens (coerce-settings (third settings))))
                "later steps keep the run's own max tokens"))
          (ok (eq :output-truncated
                  (getf (cdr (find :retry (car events) :key #'car)) :reason))))))))

(deftest truncated-tool-call-becomes-a-tool-error
  (with-agent-loop
    (let* ((events (list nil))
           (ran '())
           (agent (%write-agent
                   (%scripted (%resp "" :finish :length
                                        :calls (list (make-llm-tool-call-part
                                                      :id "c1" :name "note" :arguments "{}")
                                                     (make-llm-tool-call-part
                                                      :id "c2" :name "write"
                                                      :arguments "{\"x\":\"aa")))
                              (%resp "retrying smaller")))))
      (define-agent-tool agent "write" (:description "write") (args)
        (declare (ignore args)) (push :write ran) "ok")
      (define-agent-tool agent "note" (:description "note") (args)
        (declare (ignore args)) (push :note ran) "noted")
      (let* ((signalled nil)
             (run (handler-bind ((agent-output-truncated
                                   (lambda (c) (setf signalled c))))
                    (run-ai-agent agent "go" :on-event (%events-into events))))
             (inv (find "c2" (agent-run-invocations run)
                        :key #'agent-invocation-id :test #'equal)))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (equal '(:note) ran) "the complete call ran, the truncated one did not")
        (ok (typep signalled 'agent-output-truncated))
        (ok (eql 100 (and signalled (agent-output-truncated-max-tokens signalled))))
        (ok (and inv (eq :error (agent-invocation-status inv))))
        (ok (and inv (agent-invocation-error-p inv)))
        (ok (and inv (equal (format nil +truncated-tool-call-feedback+ "write" 100)
                            (agent-invocation-result inv))))
        (let ((tr (cdr (find :truncated (car events) :key #'car))))
          (ok (equal '("c2") (getf tr :calls)))
          (ok (eql 100 (getf tr :max-tokens))))))))

(deftest truncated-tool-call-use-value
  (with-agent-loop
    (let* ((agent (%write-agent
                   (%scripted (%resp "" :finish :length
                                        :calls (list (make-llm-tool-call-part
                                                      :id "c1" :name "write"
                                                      :arguments "{\"x\":")))
                              (%resp "ok")))))
      (define-agent-tool agent "write" (:description "write") (args)
        (declare (ignore args)) "ok")
      (let* ((run (handler-bind ((agent-output-truncated
                                   (lambda (c) (use-value "split it" c))))
                    (run-ai-agent agent "go")))
             (inv (first (agent-run-invocations run))))
        (ok (equal "split it" (agent-invocation-result inv)))))))

(deftest compacted-event-payload
  (with-agent-loop
    (let* ((events (list nil))
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b))
                                (if (find-if #'conversation-protocol:summary-turn-p turns)
                                    (%resp "fits now")
                                    (error 'llm-context-overflow :status 400
                                                                 :message "prompt is too long")))))
           (mem (conversation-protocol:make-summary-memory :backend (%summarizer)
                                                           :tail-tokens 1)))
      (run-ai-agent (make-ai-agent :name "x" :backend backend
                                   :settings (make-agent-settings :timeout 20
                                              :extra (list :compaction mem)))
                    (%history) :on-event (%events-into events))
      (let ((c (cdr (find :compacted (car events) :key #'car))))
        (ok (eq :overflow (getf c :reason)))
        (ok (= 1 (length (getf c :summary))))
        (ok (conversation-protocol:summary-turn-p (first (getf c :summary))))
        (ok (eql 1 (getf c :keep)))
        (ok (member :tokens-before c))))))

(deftest compact-agent-run-manual
  (with-agent-loop
    (let* ((events (list nil))
           (prompts '())
           (summarizer (make-mock-llm-backend
                        :handler (lambda (b turns &key &allow-other-keys)
                                   (declare (ignore b))
                                   (push (format nil "~{~a~^~%~}" (mapcar #'turn-text turns))
                                         prompts)
                                   (%resp "SUM"))))
           (mem (conversation-protocol:make-summary-memory :backend summarizer :tail-tokens 1))
           (agent (make-ai-agent :name "x"
                                 :backend (make-mock-llm-backend
                                           :handler (%scripted (%resp "a1") (%resp "a2")))
                                 :settings (make-agent-settings :timeout 20 :extra (list :compaction mem))))
           (run (run-ai-agent agent (%history) :on-event (%events-into events)))
           (did :unset))
      (compact-agent-run run :instructions "Keep the file names."
                             :callback (lambda (v) (setf did v)))
      (event-protocol:run event-protocol:*event-backend* event-protocol:*event-loop*)
      (ok (eq t did))
      (ok (find-if #'conversation-protocol:summary-turn-p (agent-run-turns run)))
      (ok (search "Keep the file names." (or (first prompts) "")))
      (ok (eq :manual (getf (cdr (find :compacted (car events) :key #'car)) :reason))))))

(deftest compact-agent-run-without-memory
  (with-agent-loop
    (let* ((run (run-ai-agent (make-ai-agent :name "x" :backend (make-mock-llm-backend)) "hi"))
           (err nil))
      (compact-agent-run run :error-callback (lambda (c) (setf err c)))
      (event-protocol:run event-protocol:*event-backend* event-protocol:*event-loop*)
      (ok (typep err 'agent-error))
      (ok (search "no compaction memory" (princ-to-string err))))))

(deftest resilience-constants
  (ok (search "cut off" +continuation-prompt+))
  (ok (search "~a" +truncated-tool-call-feedback+))
  (ok (typep (make-condition 'agent-output-truncated :max-tokens 5) 'agent-condition))
  (ng (typep (make-condition 'agent-output-truncated) 'error)))
