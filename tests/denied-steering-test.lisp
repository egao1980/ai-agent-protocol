(in-package #:ai-agent-protocol/tests)

;;; #32 a TOOL-PERMISSION-DENIED signalled by the tool handler itself is a
;;; denial (status :denied, a :permission event), not a tool error.
;;; #33 steering left in the inbox when a run finishes is reported
;;; (:undelivered) and delivered once: by CONTINUE, or by the product after
;;; DRAIN-AGENT-RUN-INBOX.

(deftest handler-signalled-denial-is-a-denial
  (with-agent-loop
    (let* ((requests (list nil))
           (events (list nil))
           (agent (make-ai-agent
                   :name "observer"
                   :backend (make-mock-llm-backend
                             :handler (%recording-handler
                                       requests
                                       (list (%calls-response (%edit-call "c1" "a.lisp"))
                                             (%text-response "ok")))))))
      (register-agent-tool
       agent
       (make-function-tool :name "edit"
                           :handler (lambda (args)
                                      (declare (ignore args))
                                      (error 'tool-permission-denied
                                             :action "edit"
                                             :resources '("lisp:mutates cl-user:*x*")
                                             :feedback "Observe mode: read only."))))
      (let* ((run (run-ai-agent agent "go"
                                :on-event (lambda (kind payload)
                                            (push (cons kind payload) (car events)))))
             (inv (find "c1" (agent-run-invocations run)
                        :key #'agent-invocation-id :test #'equal))
             (permission (find :permission (car events) :key #'car)))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (eq :denied (agent-invocation-status inv)) "status :denied, not :error")
        (ok (agent-invocation-error-p inv) "the model still gets an error result")
        (ok (search "was not run: permission denied" (agent-invocation-result inv)))
        (ok (search "Observe mode: read only." (agent-invocation-result inv)))
        (ok permission "a :permission event")
        (ok (eq inv (getf (cdr permission) :invocation)))
        (ok (typep (getf (cdr permission) :condition) 'tool-permission-denied))
        (let ((part (%tool-result-for (first (car requests)) "c1")))
          (ok part "the next request carries the tool result")
          (ok (search "Observe mode" (llm-tool-result-part-content part))))))))

(deftest handler-signalled-error-stays-an-error
  (with-agent-loop
    (let* ((requests (list nil))
           (agent (make-ai-agent
                   :name "failing"
                   :backend (make-mock-llm-backend
                             :handler (%recording-handler
                                       requests
                                       (list (%calls-response (%edit-call "c1" "a.lisp"))
                                             (%text-response "ok")))))))
      (register-agent-tool
       agent
       (make-function-tool :name "edit"
                           :handler (lambda (args)
                                      (declare (ignore args))
                                      (error "disk full"))))
      (let* ((events (list nil))
             (run (run-ai-agent agent "go"
                                :on-event (lambda (kind payload)
                                            (push (cons kind payload) (car events)))))
             (inv (first (agent-run-invocations run))))
        (ok (eq :error (agent-invocation-status inv)))
        (ok (null (find :permission (car events) :key #'car)))))))

(defun %steering-agent (requests run-box)
  "Max one step: step 1 calls a tool, the wrap-up generate posts \"steer\" to
the run (the user typed during the last step)."
  (let ((agent (make-ai-agent
                :name "steered"
                :settings (make-agent-settings :max-steps 1)
                :backend (make-mock-llm-backend
                          :handler (lambda (backend turns &key &allow-other-keys)
                                     (declare (ignore backend))
                                     (push (copy-list turns) (car requests))
                                     (case (length (car requests))
                                       (1 (%calls-response (%edit-call "c1" "a.lisp")))
                                       (2 (agent-run-post (car run-box) "steer")
                                        (%text-response "wrapped up"))
                                       (t (%text-response "next"))))))))
    (register-agent-tool
     agent
     (make-function-tool :name "edit"
                         :handler (lambda (args) (declare (ignore args)) "edited")))
    agent))

(defun %user-text-count (turns text)
  (count-if (lambda (turn)
              (and (eq :user (llm-turn-role turn))
                   (some (lambda (p) (and (llm-text-part-p p)
                                          (equal text (llm-text-part-text p))))
                         (llm-turn-parts turn))))
            turns))

(deftest steering-left-at-finish-is-reported-and-delivered-once
  (with-agent-loop
    (testing "CONTINUE delivers the undelivered steering"
      (let* ((requests (list nil))
             (box (list nil))
             (undelivered (list nil))
             (run (run-ai-agent (%steering-agent requests box) "go"
                                :on-event (lambda (kind payload)
                                            (case kind
                                              (:started (setf (car box) payload))
                                              (:undelivered
                                               (setf (car undelivered) payload)))))))
        (ok (eq :max-steps (agent-run-finish-reason run)))
        (ok (= 1 (length (car undelivered))) ":undelivered names the queued turn")
        (ok (eq :user (llm-turn-role (first (car undelivered)))))
        (ok (= 1 (length (agent-run-inbox run))) "it stays queued")
        (continue-ai-agent run "follow-up")
        (ok (null (agent-run-inbox run)))
        (ok (= 1 (%user-text-count (first (car requests)) "steer"))
            "delivered once")
        (ok (= 1 (%user-text-count (agent-run-turns run) "steer")))))
    (testing "the product drains it and sends it as the follow-up"
      (let* ((requests (list nil))
             (box (list nil))
             (run (run-ai-agent (%steering-agent requests box) "go"
                                :on-event (lambda (kind payload)
                                            (when (eq kind :started)
                                              (setf (car box) payload)))))
             (taken (drain-agent-run-inbox run)))
        (ok (= 1 (length taken)))
        (ok (null (agent-run-inbox run)))
        (ok (null (drain-agent-run-inbox run)) "a second take is empty")
        (continue-ai-agent run (first taken))
        (ok (= 1 (%user-text-count (first (car requests)) "steer"))
            "sent once, as the follow-up")))
    (testing "a post after the drain is delivered by CONTINUE"
      (let* ((requests (list nil))
             (box (list nil))
             (run (run-ai-agent (%steering-agent requests box) "go"
                                :on-event (lambda (kind payload)
                                            (when (eq kind :started)
                                              (setf (car box) payload)))))
             (taken (drain-agent-run-inbox run)))
        (agent-run-post run "later")
        (continue-ai-agent run (first taken))
        (ok (= 1 (%user-text-count (first (car requests)) "steer")))
        (ok (= 1 (%user-text-count (first (car requests)) "later")))))
    (testing "no :undelivered event when the inbox is empty"
      (let* ((events (list nil))
             (agent (make-ai-agent :backend (make-mock-llm-backend))))
        (run-ai-agent agent "hi" :on-event (lambda (kind payload)
                                             (declare (ignore payload))
                                             (push kind (car events))))
        (ok (null (member :undelivered (car events))))))))
