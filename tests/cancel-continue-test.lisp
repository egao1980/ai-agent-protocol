(in-package #:ai-agent-protocol/tests)

;;; #29 continuing after a cancel starts a fresh turn (flag cleared, no stale
;;; in-flight turn); an "always" reply whose rule save fails still approves.
;;; #30 a denial says who decided (user, policy, unattended).

(defun %cancel-first-part (run-box)
  "ON-EVENT that cancels the run on the first streamed :part, once."
  (let ((done nil))
    (lambda (kind payload)
      (case kind
        (:started (setf (car run-box) payload))
        (:part (unless done
                 (setf done t)
                 (cancel-agent-run (car run-box))))))))

(defun %role-texts (turns)
  "(role . text) for the user and assistant turns of TURNS."
  (loop for turn in turns
        when (member (llm-turn-role turn) '(:user :assistant))
          collect (cons (llm-turn-role turn)
                        (format nil "~{~a~}"
                                (loop for p in (llm-turn-parts turn)
                                      when (llm-text-part-p p)
                                        collect (llm-text-part-text p))))))

(deftest continue-after-cancel-is-a-fresh-turn
  (with-agent-loop
    (let* ((requests (list nil))
           (agent (make-ai-agent
                   :name "chat"
                   :backend (make-mock-llm-backend
                             :handler (%recording-handler
                                       requests
                                       (list (make-llm-response
                                              :parts (list (make-llm-text-part :text "partial")
                                                           (make-llm-tool-call-part
                                                            :id "c1" :name "edit" :arguments "{}"))
                                              :finish-reason :tool-use)
                                             (%text-response "two")
                                             (%text-response "three"))))))
           (box (list nil))
           (run (run-ai-agent agent "one" :on-event (%cancel-first-part box))))
      (ok (eq :canceled (agent-run-finish-reason run)))
      (ok (null (agent-run-in-flight-turn run)) "nothing left in flight")
      (ok (notany (lambda (turn) (some #'llm-tool-call-part-p (llm-turn-parts turn)))
                  (agent-run-turns run))
          "the canceled step's tool call (no result) is not in history")
      (continue-ai-agent run "second")
      (ok (eq :stop (agent-run-finish-reason run)) "the continued turn is not canceled")
      (ok (not (agent-run-handle-canceled-p (agent-run-handle run))))
      (continue-ai-agent run "third")
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal '((:user . "one") (:assistant . "partial")
                   (:user . "second") (:assistant . "two")
                   (:user . "third") (:assistant . "three"))
                 (%role-texts (agent-run-turns run)))
          "history keeps its order"))))

(deftest continue-after-cancel-workaround-still-works
  ;; Products on 0.12.0 reset the flag themselves before continuing.
  (with-agent-loop
    (let* ((agent (make-ai-agent :backend (make-mock-llm-backend)))
           (box (list nil))
           (run (run-ai-agent agent "hi" :on-event (%cancel-first-part box))))
      (ok (eq :canceled (agent-run-finish-reason run)))
      (setf (agent-run-handle-canceled-p (agent-run-handle run)) nil)
      (continue-ai-agent run "again")
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "echo: again" (agent-run-text run))))))

(defclass failing-rule-store (permission-rule-store) ())

(defmethod save-permission-rule ((store failing-rule-store) rule &key agent)
  (declare (ignore rule agent))
  (error "settings file is read-only"))

(defmethod list-permission-rules ((store failing-rule-store) &key agent)
  (declare (ignore agent))
  nil)

(defun %failing-store-run (responses)
  (let* ((agent (%ask-edit-agent (list nil) responses
                                 :store (make-instance 'failing-rule-store))))
    (run-ai-agent agent "go")))

(deftest approve-always-save-failure-still-approves
  (with-agent-loop
    (testing "the error reaches the caller; the call is approved"
      (let ((run (%failing-store-run
                  (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                         (%edit-call "c2" "src/a.lisp"))
                        (%text-response "done")))))
        (ok (signals (approve-invocation run (%invocation run "c1") :always t)))
        (ok (eq :approved (agent-invocation-status (%invocation run "c1"))))
        (ok (not (member (%invocation run "c1") (agent-run-pending run))))
        (ok (= 1 (length (agent-run-approved-permissions run)))
            "the rule still holds for the run")))
    (testing "SKIP-SAVE keeps the rule for the run"
      (let ((run (%failing-store-run
                  (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                         (%edit-call "c2" "src/a.lisp"))
                        (%text-response "done")))))
        (handler-bind ((agent-permission-save-error (lambda (c) (invoke-skip-save c))))
          (approve-invocation run (%invocation run "c1") :always t))
        (ok (eq :approved (agent-invocation-status (%invocation run "c1"))))
        (ok (eq :approved (agent-invocation-status (%invocation run "c2")))
            "the matching pending call is approved too")
        (ok (eq :stop (agent-run-finish-reason (resume-ai-agent run))))))
    (testing "APPROVE-ONCE drops the rule"
      (let ((run (%failing-store-run
                  (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                         (%edit-call "c2" "src/a.lisp"))
                        (%text-response "done")))))
        (handler-bind ((agent-permission-save-error (lambda (c) (invoke-approve-once c))))
          (approve-invocation run (%invocation run "c1") :always t))
        (ok (eq :approved (agent-invocation-status (%invocation run "c1"))))
        (ok (eq :proposed (agent-invocation-status (%invocation run "c2"))))
        (ok (null (agent-run-approved-permissions run)))))
    (testing "the APPROVE-ALWAYS restart"
      (let* ((agent (%ask-edit-agent (list nil)
                                     (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                           (%text-response "done"))
                                     :store (make-instance 'failing-rule-store)))
             (run (handler-bind ((agent-approval-required
                                   (lambda (c) (invoke-approve-always c)))
                                 (agent-permission-save-error
                                   (lambda (c) (invoke-skip-save c))))
                    (run-ai-agent agent "go"))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (eq :done (agent-invocation-status (%invocation run "c1"))))))))

(deftest denial-says-who-decided
  (with-agent-loop
    (flet ((result-text (handler)
             (let* ((requests (list nil))
                    (run (handler-bind ((agent-approval-required handler))
                           (run-ai-agent (%denial-agent requests) "go"))))
               (ok (eq :stop (agent-run-finish-reason run)))
               (llm-tool-result-part-content
                (%tool-result-for (first (car requests)) "c1")))))
      (let ((text (result-text (lambda (c) (invoke-deny c "Use --allowed-tools."
                                                        :by :unattended)))))
        (ok (search "approval is not available" text))
        (ok (not (search "the user rejected" text)))
        (ok (search "Use --allowed-tools." text)))
      (let ((text (result-text (lambda (c) (invoke-deny c nil :by :policy)))))
        (ok (search "permission denied" text))
        (ok (not (search "the user rejected" text))))
      (let ((text (result-text (lambda (c) (invoke-deny-all c "Not now." :by :unattended)))))
        (ok (search "approval is not available" text)))
      (ok (search "the user rejected the request"
                  (result-text (lambda (c) (invoke-deny c "No."))))
          "a person's answer reads as before")
      (ok (equal "denied" (result-text (lambda (c) (invoke-deny c))))))
    (let* ((requests (list nil))
           (run (run-ai-agent (%denial-agent requests) "go"))
           (inv (first (agent-run-pending run))))
      (deny-invocation run inv :by :unattended)
      (ok (search "approval is not available" (agent-invocation-result inv)))
      (resume-ai-agent run))
    (ok (eq :unattended (tool-permission-denied-decision
                         (make-condition 'tool-permission-denied
                                         :decision (denial-decision :unattended)))))
    (ok (signals (denial-decision :robot)))))

;;; cl-stack-code#59: a cancel while the generate is in flight finishes the
;;; run at once; the generate's late parts and response are dropped.

(defun %blocking-handler (release &key (text "late answer") (limit 10))
  "Mock handler that holds the generate until (CAR RELEASE) is true (or LIMIT s)."
  (lambda (backend turns &rest keys)
    (declare (ignore backend turns keys))
    (loop with deadline = (+ (get-internal-real-time)
                             (* limit internal-time-units-per-second))
          until (or (car release) (> (get-internal-real-time) deadline))
          do (sleep 0.02))
    (make-llm-response :parts (list (make-llm-text-part :text text))
                       :finish-reason :stop)))

(defun %drain-loop (seconds)
  "Run the bound loop for SECONDS so late deliveries land."
  (let ((eb event-protocol:*event-backend*)
        (el event-protocol:*event-loop*))
    (event-protocol:sleep* eb el seconds
                           :callback (lambda () (event-protocol:stop eb el)))
    (event-protocol:run eb el :stop-when-idle nil)))

(deftest cancel-during-in-flight-generate-finishes-at-once
  (with-agent-loop
    (let* ((release (list nil))
           (events (list nil))
           (box (list nil))
           (agent (make-ai-agent :name "slow"
                                 :backend (make-mock-llm-backend
                                           :handler (%blocking-handler release))))
           (start (get-internal-real-time))
           (run (progn
                  (event-protocol:sleep* event-protocol:*event-backend*
                                         event-protocol:*event-loop* 0.3
                                         :callback (lambda ()
                                                     (when (car box)
                                                       (cancel-agent-run (car box)))))
                  (run-ai-agent agent "hello"
                                :on-event (lambda (kind payload)
                                            (when (eq kind :started)
                                              (setf (car box) payload))
                                            (push kind (car events))))))
           (elapsed (/ (- (get-internal-real-time) start)
                       internal-time-units-per-second)))
      (unwind-protect
          (progn
            (ok (eq :canceled (agent-run-finish-reason run)))
            (ok (< elapsed 3) (format nil "returned after ~,2f s, not after the request" elapsed))
            (ok (null (agent-run-in-flight-turn run)))
            (setf (car release) t)
            (%drain-loop 0.5)
            (ok (eq :canceled (agent-run-finish-reason run))
                "the late response does not reopen the run")
            (ok (notany (lambda (turn)
                          (and (eq :assistant (llm-turn-role turn))
                               (some (lambda (p)
                                       (and (llm-text-part-p p)
                                            (search "late answer" (llm-text-part-text p))))
                                     (llm-turn-parts turn))))
                        (agent-run-turns run))
                "the late response is not committed")
            (ok (= 1 (count :finished (car events))) "finished once")
            (ok (not (member :part (car events))) "no late :part events")
            (ok (not (member :response (car events))) "no late :response event"))
        (setf (car release) t)))))

(deftest continue-after-in-flight-cancel-generates-again
  (with-agent-loop
    (let* ((release (list nil))
           (box (list nil))
           (calls (list 0))
           (blocking (%blocking-handler release))
           (agent (make-ai-agent
                   :name "slow"
                   :backend (make-mock-llm-backend
                             :handler (lambda (backend turns &rest keys)
                                        (if (= 1 (incf (car calls)))
                                            (apply blocking backend turns keys)
                                            (make-llm-response
                                             :parts (list (make-llm-text-part :text "fresh"))
                                             :finish-reason :stop))))))
           (run (progn
                  (event-protocol:sleep* event-protocol:*event-backend*
                                         event-protocol:*event-loop* 0.3
                                         :callback (lambda ()
                                                     (when (car box)
                                                       (cancel-agent-run (car box)))))
                  (run-ai-agent agent "hello"
                                :on-event (lambda (kind payload)
                                            (when (eq kind :started)
                                              (setf (car box) payload)))))))
      (unwind-protect
          (progn
            (ok (eq :canceled (agent-run-finish-reason run)))
            (setf (car release) t)
            (%drain-loop 0.3)
            (continue-ai-agent run "again")
            (ok (eq :stop (agent-run-finish-reason run)))
            (ok (equal "fresh" (agent-run-text run))))
        (setf (car release) t)))))
