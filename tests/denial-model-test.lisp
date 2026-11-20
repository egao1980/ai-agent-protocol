(in-package #:ai-agent-protocol/tests)

;;; #27 a denied call gets its tool result before the next request;
;;; #26 the denial text names only the denied resources;
;;; #25 the agent's :model reaches the backend.

(defun %recording-handler (requests responses &key models)
  "Answer RESPONSES in turn (the last one repeats). Push each request's turns
onto (car REQUESTS) and its :model onto (car MODELS)."
  (lambda (backend turns &key model &allow-other-keys)
    (declare (ignore backend))
    (push (copy-list turns) (car requests))
    (when models (push model (car models)))
    (let ((i (1- (length (car requests)))))
      (if (< i (length responses))
          (elt responses i)
          (car (last responses))))))

(defun %tool-result-for (turns id)
  "The tool-result part for call ID in TURNS, or NIL."
  (loop for turn in turns
        when (eq :tool (llm-turn-role turn))
          do (loop for part in (llm-turn-parts turn)
                   when (and (llm-tool-result-part-p part)
                             (equal id (llm-tool-result-part-id part)))
                     do (return-from %tool-result-for part))))

(defun %denial-agent (requests &key (permissions '(("edit" "*" :ask))))
  (let ((agent (make-ai-agent
                :name "coder"
                :backend (make-mock-llm-backend
                          :handler (%recording-handler
                                    requests
                                    (list (%calls-response (%edit-call "c1" "src/a.lisp"))
                                          (%text-response "done"))))
                :permissions permissions)))
    (register-agent-tool
     agent
     (make-function-tool :name "edit"
                         :permission-resources (lambda (args) (list (%path-of args)))
                         :handler (lambda (args) (declare (ignore args)) "edited")))
    agent))

(deftest handler-denial-writes-a-tool-result
  (with-agent-loop
    (testing "INVOKE-DENY with feedback"
      (let* ((requests (list nil))
             (run (handler-bind ((agent-approval-required
                                   (lambda (c) (invoke-deny c "Ask the user first."))))
                    (run-ai-agent (%denial-agent requests) "go"))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (= 2 (length (car requests))))
        (let* ((next (first (car requests)))
               (last-turn (car (last next)))
               (part (%tool-result-for next "c1")))
          (ok (eq :tool (llm-turn-role last-turn))
              "the next request ends with the tool result")
          (ok part "the denied call id has a tool result")
          (ok (search "Ask the user first." (llm-tool-result-part-content part)))
          (ok (= 1 (count-if (lambda (tu) (%tool-result-for (list tu) "c1")) next))
              "recorded once"))))
    (testing "INVOKE-DENY without feedback"
      (let* ((requests (list nil))
             (run (handler-bind ((agent-approval-required
                                   (lambda (c) (invoke-deny c))))
                    (run-ai-agent (%denial-agent requests) "go"))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (let ((part (%tool-result-for (first (car requests)) "c1")))
          (ok part)
          (ok (equal "denied" (llm-tool-result-part-content part))))))
    (testing "INVOKE-DENY-ALL"
      (let* ((requests (list nil))
             (run (handler-bind ((agent-approval-required
                                   (lambda (c) (invoke-deny-all c "Not now."))))
                    (run-ai-agent (%denial-agent requests) "go"))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (let ((part (%tool-result-for (first (car requests)) "c1")))
          (ok part)
          (ok (search "Not now." (llm-tool-result-part-content part))))))
    (testing "HITL deny-invocation then resume"
      (let* ((requests (list nil))
             (run (run-ai-agent (%denial-agent requests) "go")))
        (ok (eq :approval (agent-run-finish-reason run)))
        (deny-invocation run (first (agent-run-pending run)) :feedback "No.")
        (let ((run2 (resume-ai-agent run)))
          (ok (eq :stop (agent-run-finish-reason run2)))
          (let ((part (%tool-result-for (first (car requests)) "c1")))
            (ok part)
            (ok (search "No." (llm-tool-result-part-content part)))))))))

(deftest permission-denied-names-only-denied-resources
  (let* ((tool (make-function-tool
                :name "bash"
                :permission-resources
                (lambda (args)
                  (declare (ignore args))
                  (list "bash:ls && rm x" "program:ls" "program:rm" "write:x"))))
         (rules '(("bash" "bash:*" :allow)
                  ("bash" "program:ls" :allow)
                  ("bash" "write:*" :allow)
                  ("bash" "program:rm" :deny))))
    (with-agent-loop
      (let* ((requests (list nil))
             (agent2 (make-ai-agent
                      :name "a" :permissions rules
                      :backend (make-mock-llm-backend
                                :handler (%recording-handler
                                          requests
                                          (list (make-llm-response
                                                 :parts (list (make-llm-tool-call-part
                                                               :id "b1" :name "bash"
                                                               :arguments "{}"))
                                                 :finish-reason :tool-use)
                                                (%text-response "done")))))))
        (register-agent-tool agent2 tool)
        (let* ((run (run-ai-agent agent2 "go"))
               (inv (find "b1" (agent-run-invocations run)
                          :key #'agent-invocation-id :test #'equal))
               (text (agent-invocation-result inv)))
          (ok (eq :denied (agent-invocation-status inv)))
          (ok (search "permission denied for program:rm" text) text)
          (ok (not (search "program:ls" text)) "allowed resources are not named"))))
    (testing "the condition keeps the full list"
      (let ((c (make-condition 'tool-permission-denied
                               :resources '("program:rm")
                               :call-resources '("program:ls" "program:rm"))))
        (ok (equal '("program:ls" "program:rm")
                   (tool-permission-denied-call-resources c)))))))

(deftest agent-model-reaches-the-backend
  (with-agent-loop
    (flet ((agent (models &rest args)
             (apply #'make-ai-agent
                    :name "m"
                    :backend (make-mock-llm-backend
                              :handler (%recording-handler (list nil)
                                                           (list (%text-response "hi"))
                                                           :models models))
                    args)))
      (testing "AI-AGENT :model"
        (let ((models (list nil)))
          (run-ai-agent (agent models :model "cheap-model") "go")
          (ok (equal '("cheap-model") (car models)))))
      (testing "run settings :model overrides it"
        (let* ((models (list nil))
               (a (agent models :model "cheap-model")))
          (ok (equal "cheap-model" (ai-agent-model a)))
          (run-ai-agent a "go" :settings '(:model "big-model"))
          (ok (equal '("big-model") (car models)))))
      (testing "no model → NIL (the backend default)"
        (let ((models (list nil)))
          (run-ai-agent (agent models) "go")
          (ok (equal '(nil) (car models))))))))
