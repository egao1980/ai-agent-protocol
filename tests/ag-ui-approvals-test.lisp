(in-package #:ai-agent-protocol/tests)

;;; #22: the AG-UI side of the "always" and reject-all approval replies.
;;; Uses the edit agent from permission-approvals-test.

(defun %resume-input (&rest entries)
  (ag-ui-protocol:make-run-agent-input :thread-id "t1" :run-id "r2"
                                       :resume entries))

(defun %answer (id &rest payload)
  "Resume entry answering the interrupt for call ID with PAYLOAD (key value …)."
  (ag-ui-protocol:make-resume-entry
   :interrupt-id (format nil "int-~a" id)
   :payload (apply #'ag-ui-protocol:json-object payload)))

(deftest ag-ui-interrupt-advertises-approval-replies
  (with-agent-loop
    (let* ((agent (%ask-edit-agent (list nil)
                                   (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                         (%text-response "done"))))
           (run (run-ai-agent agent "go"))
           (int (first (ai-agent-protocol/ag-ui:run-interrupts run)))
           (props (gethash "properties"
                           (ag-ui-protocol:interrupt-response-schema int))))
      (ok (gethash "always" props))
      (ok (gethash "rejectAll" props))
      (ok (gethash "feedback" props))
      (ok (equalp #("once" "always" "reject" "rejectAll")
                  (gethash "replies" (ag-ui-protocol:interrupt-metadata int))))
      (testing "the advertised payloads pass the responseSchema"
        (ok (ag-ui-protocol:validate-resume
             (list int) (list (%answer "c1" "approved" t "always" t))))
        (ok (ag-ui-protocol:validate-resume
             (list int) (list (%answer "c1" "approved" nil "rejectAll" t
                                       "feedback" "No."))))))))

(deftest ag-ui-resume-always-allows-later-matching-calls
  (with-agent-loop
    (let* ((ran (list nil))
           (store (make-in-memory-permission-rule-store))
           (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                             (%calls-response (%edit-call "c2" "src/a.lisp"))
                                             (%text-response "done"))
                                   :store store))
           (run (run-ai-agent agent "go")))
      (ok (eq :approval (agent-run-finish-reason run)))
      (ai-agent-protocol/ag-ui:resume-ag-ui-agent-run
       run (%resume-input (%answer "c1" "approved" t "always" t)))
      (ok (eq :stop (agent-run-finish-reason run))
          "the second matching call ran without a new interrupt")
      (ok (equal '("src/a.lisp" "src/a.lisp") (car ran)))
      (ok (= 1 (length (agent-run-approved-permissions run))))
      (ok (= 1 (length (list-permission-rules store))) "the rule is saved"))))

(deftest ag-ui-resume-approved-without-always-stays-once
  (with-agent-loop
    (let* ((agent (%ask-edit-agent (list nil)
                                   (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                         (%calls-response (%edit-call "c2" "src/a.lisp"))
                                         (%text-response "done"))))
           (run (run-ai-agent agent "go")))
      (ai-agent-protocol/ag-ui:resume-ag-ui-agent-run
       run (%resume-input (%answer "c1" "approved" t "always" nil)))
      (ok (eq :approval (agent-run-finish-reason run)))
      (ok (null (agent-run-approved-permissions run))))))

(deftest ag-ui-resume-explicit-answer-beats-another-calls-always
  (with-agent-loop
    (let* ((ran (list nil))
           (agent (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                                              (%edit-call "c2" "src/a.lisp"))
                                             (%text-response "done"))))
           (run (run-ai-agent agent "go")))
      (ai-agent-protocol/ag-ui:apply-resume
       run (%resume-input (%answer "c1" "approved" t "always" t)
                          (%answer "c2" "approved" nil)))
      (ok (eq :approved (agent-invocation-status (%invocation run "c1"))))
      (ok (eq :denied (agent-invocation-status (%invocation run "c2")))))))

(deftest ag-ui-resume-reject-all
  (with-agent-loop
    (flet ((three-edits (ran)
             (%ask-edit-agent ran (list (%calls-response (%edit-call "c1" "src/a.lisp")
                                                         (%edit-call "c2" "src/b.lisp")
                                                         (%edit-call "c3" "src/c.lisp"))
                                        (%text-response "done")))))
      (testing "one rejectAll entry answers every open interrupt"
        (let* ((ran (list nil))
               (run (run-ai-agent (three-edits ran) "go")))
          (ok (= 3 (length (ai-agent-protocol/ag-ui:run-interrupts run))))
          (ok (= 3 (ai-agent-protocol/ag-ui:apply-resume
                    run (%resume-input (%answer "c2" "approved" nil "rejectAll" t
                                                "feedback" "Explain first.")))))
          (ok (null (agent-run-pending run)))
          (dolist (id '("c1" "c2" "c3"))
            (let ((inv (%invocation run id)))
              (ok (eq :denied (agent-invocation-status inv)))
              (ok (search "Explain first." (agent-invocation-result inv)))))
          (ok (eq :stop (agent-run-finish-reason (resume-ai-agent run))))
          (ok (null (car ran)))))
      (testing "rejectAll wins over an explicit approval of another call"
        (let* ((ran (list nil))
               (run (run-ai-agent (three-edits ran) "go")))
          (ai-agent-protocol/ag-ui:apply-resume
           run (%resume-input (%answer "c1" "approved" t)
                              (%answer "c3" "approved" nil "rejectAll" t)))
          (ok (every (lambda (inv) (eq :denied (agent-invocation-status inv)))
                     (agent-run-invocations run)))))
      (testing "without rejectAll every interrupt must still be answered"
        (let ((run (run-ai-agent (three-edits (list nil)) "go")))
          (ok (signals (ai-agent-protocol/ag-ui:apply-resume
                        run (%resume-input (%answer "c1" "approved" nil)))
                       'ag-ui-protocol:ag-ui-resume-error))
          (ok (= 3 (length (agent-run-pending run))))))
      (testing "rejectAll still refuses an unknown interrupt"
        (let ((run (run-ai-agent (three-edits (list nil)) "go")))
          (ok (signals (ai-agent-protocol/ag-ui:apply-resume
                        run (%resume-input (%answer "nope" "approved" nil
                                                    "rejectAll" t)))
                       'ag-ui-protocol:ag-ui-resume-error))
          (ok (= 3 (length (agent-run-pending run)))))))))

(deftest ag-ui-resume-deny-feedback-reaches-the-model
  (with-agent-loop
    (let* ((agent (%ask-edit-agent (list nil)
                                   (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                         (%text-response "done"))))
           (run (run-ai-agent agent "go")))
      (ai-agent-protocol/ag-ui:apply-resume
       run (%resume-input (%answer "c1" "approved" nil "feedback" "Use b.lisp.")))
      (let ((inv (%invocation run "c1")))
        (ok (eq :denied (agent-invocation-status inv)))
        (ok (agent-invocation-error-p inv))
        (ok (search "Use b.lisp." (agent-invocation-result inv)))))))
