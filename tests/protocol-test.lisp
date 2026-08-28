(in-package #:ai-agent-protocol/tests)

(defmacro with-agent-loop (&body body)
  `(let* ((eb (event-backend-libuv:make-libuv-backend))
          (el (event-protocol:make-event-loop eb)))
     (event-protocol:with-event-backend (eb)
       (event-protocol:with-event-loop-var (el)
         ,@body))))

(defun %one-shot-tools (calls then-text)
  (let ((fired nil))
    (lambda (backend turns &key &allow-other-keys)
      (declare (ignore backend turns))
      (if fired
          (make-llm-response
           :parts (list (make-llm-text-part :text then-text))
           :finish-reason :stop)
          (progn
            (setf fired t)
            (make-llm-response
             :parts (mapcar (lambda (c)
                              (if (llm-tool-call-part-p c)
                                  c
                                  (make-llm-tool-call-part
                                   :id (getf c :id) :name (getf c :name)
                                   :arguments (or (getf c :arguments) "{}"))))
                            calls)
             :finish-reason :tool-use))))))

(deftest echo-no-tools
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend))
           (agent (make-ai-agent :name "echo" :backend backend
                                 :instructions "Be brief."))
           (run (run-ai-agent agent "hi")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "echo: hi" (agent-run-text run)))
      (ok (find :system (agent-run-turns run) :key #'llm-turn-role)))))

(deftest function-tool-one-shot
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "sum" :arguments "{}"))
                               "3")))
           (agent (make-ai-agent :name "math" :backend backend))
           (got nil))
      (define-agent-tool agent "sum" (:description "add") (args)
        (declare (ignore args))
        (setf got t)
        "3")
      (let ((run (run-ai-agent agent "1+2")))
        (ok got)
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (equal "3" (agent-run-text run)))
        (ok (= 1 (length (agent-run-invocations run))))
        (ok (eq :done (agent-invocation-status (first (agent-run-invocations run)))))))))

(deftest approval-pauses-then-resume
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
        (ok (= 1 (length (agent-run-pending run))))
        (approve-invocation run (first (agent-run-pending run)))
        (let ((run2 (resume-ai-agent run)))
          (ok (eq :stop (agent-run-finish-reason run2)))
          (ok (equal "ok" (agent-run-text run2)))
          (ok (equal "did-it"
                     (agent-invocation-result
                      (first (agent-run-invocations run2))))))))))

(deftest nested-agent-as-tool
  (with-agent-loop
    (let* ((child (make-ai-agent
                   :name "researcher"
                   :backend (make-mock-llm-backend :prefix "notes: ")
                   :instructions "Research."))
           (parent-backend
            (make-mock-llm-backend
             :handler (%one-shot-tools
                       (list (make-llm-tool-call-part
                              :id "c1" :name "researcher" :arguments "q"))
                       "done")))
           (parent (make-ai-agent :name "manager" :backend parent-backend
                                  :tools (list child)))
           (run (run-ai-agent parent "ask")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "done" (agent-run-text run)))
      (ok (equal "notes: q"
                 (agent-invocation-result (first (agent-run-invocations run))))))))

(deftest handoff-takes-the-run
  (with-agent-loop
    (let* ((specialist (make-ai-agent
                        :name "writer"
                        :backend (make-mock-llm-backend :prefix "draft: ")))
           (manager (make-ai-agent
                     :name "triage"
                     :backend (make-mock-llm-backend
                               :handler (%one-shot-tools
                                         (list (make-llm-tool-call-part
                                                :id "c1" :name "writer"
                                                :arguments "{}"))
                                         "unused"))
                     :handoffs (list specialist)))
           (run (run-ai-agent manager "write")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (eq specialist (agent-run-agent run)))
      (ok (equal "draft: write" (agent-run-text run))))))

(deftest defagent-defaults
  (defagent tester ()
    "A test agent."
    (:name "tester")
    (:instructions "Test."))
  (let ((a (make-instance 'tester)))
    (ok (equal "tester" (ai-agent-name a)))
    (ok (equal "Test." (ai-agent-instructions a)))))

(deftest max-steps-stops
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :tool-calls (list (make-llm-tool-call-part
                                        :id "c1" :name "loop" :arguments "{}"))))
           (agent (make-ai-agent
                   :name "loopy" :backend backend
                   :settings (make-agent-settings :max-steps 2))))
      (define-agent-tool agent "loop" () (args)
        (declare (ignore args))
        "again")
      (let ((run (run-ai-agent agent "go")))
        (ok (eq :max-steps (agent-run-finish-reason run)))
        (ok (= 2 (agent-run-step run)))))))

(deftest cancel-before-tick
  (with-agent-loop
    (let* ((agent (make-ai-agent :backend (make-mock-llm-backend)))
           (handle nil)
           (run nil))
      (setf handle
            (run-ai-agent-async
             agent "hi"
             :callback (lambda (r) (setf run r))
             :error-callback #'error))
      (cancel-agent-run handle)
      (event-protocol:run event-protocol:*event-backend* event-protocol:*event-loop*
                          :stop-when-idle t)
      (ok (or (null run)
              (eq :canceled (agent-run-finish-reason run)))))))

(deftest stream-parts-and-on-part
  (with-agent-loop
    (let* ((kinds '())
           (parts '())
           (backend (make-mock-llm-backend
                     :handler (lambda (b turns &key &allow-other-keys)
                                (declare (ignore b turns))
                                (make-llm-response
                                 :parts (list (make-llm-text-part :text "hel")
                                              (make-llm-text-part :text "lo"))
                                 :finish-reason :stop))))
           (agent (make-ai-agent :name "echo" :backend backend))
           (run (run-ai-agent agent "hi"
                              :on-event (lambda (k p)
                                          (push k kinds)
                                          (when (eq k :part) (push p parts)))
                              :on-part (lambda (p) (push (llm-text-part-text p) parts)))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal '(:started :step :part :part :response :finished)
                 (reverse kinds)))
      (ok (find "hel" parts :test #'equal))
      (ok (find "lo" parts :test #'equal))
      (ok (equal "hello" (agent-run-text run)))
      (let ((asst (find :assistant (agent-run-turns run) :key #'llm-turn-role :from-end t)))
        (ok asst)
        (ok (= 2 (length (llm-turn-parts asst))))))))

(deftest tool-call-argument-suffix
  (with-agent-loop
    (let* ((seen-args nil)
           (backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "sum" :arguments "{\"a\":")
                                     (make-llm-tool-call-part
                                      :id "c1" :name "sum" :arguments "1}"))
                               "3")))
           (agent (make-ai-agent :name "math" :backend backend)))
      (define-agent-tool agent "sum" () (args)
        (setf seen-args args)
        "3")
      (let ((run (run-ai-agent agent "1")))
        (ok (equal "{\"a\":1}" seen-args))
        (ok (= 1 (length (agent-run-invocations run))))
        (ok (equal "{\"a\":1}"
                   (agent-invocation-arguments
                    (first (agent-run-invocations run)))))))))

(defclass %generate-only-backend (llm-backend) ())

(defmethod generate ((backend %generate-only-backend) turns &key &allow-other-keys)
  (declare (ignore turns))
  (make-llm-response :parts (list (make-llm-text-part :text "only-gen"))
                     :finish-reason :stop))

(deftest generate-only-fallback-emits-parts
  (with-agent-loop
    (let* ((parts '())
           (agent (make-ai-agent :backend (make-instance '%generate-only-backend)))
           (run (run-ai-agent agent "hi"
                              :on-part (lambda (p) (push p parts)))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "only-gen" (agent-run-text run)))
      (ok (= 1 (length parts)))
      (ok (llm-text-part-p (first parts))))))

(deftest invocation-events-on-tool
  (with-agent-loop
    (let* ((statuses '())
           (backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part
                                      :id "c1" :name "sum" :arguments "{}"))
                               "3")))
           (agent (make-ai-agent :backend backend)))
      (define-agent-tool agent "sum" () (args)
        (declare (ignore args))
        "3")
      (run-ai-agent agent "1+2"
                    :on-event (lambda (k p)
                                (when (eq k :invocation)
                                  (push (agent-invocation-status p) statuses))))
      (ok (equal '(:approved :running :done) (reverse statuses))))))
