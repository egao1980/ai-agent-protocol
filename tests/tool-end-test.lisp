(in-package #:ai-agent-protocol/tests)

;;; 0.16.1 (#51): a tool output with :END-RUN ends the run once every call of
;;; the step is recorded (finish reason :TOOL-END).

(defun %ender-agent (n responses &key (fail nil))
  "An agent whose \"propose\" tool ends the run (or signals when FAIL) and
whose \"look\" tool answers \"seen\". N's car counts generates."
  (let ((agent (make-ai-agent :name "ender"
                              :backend (make-mock-llm-backend
                                        :handler (%counting-handler n responses)))))
    (define-agent-tool agent "propose" (:description "propose and stop") (args)
      (declare (ignore args))
      (if fail
          (error 'tool-input-error :message "bad plan")
          (make-agent-tool-output :text "saved" :end-run t)))
    (define-agent-tool agent "look" (:description "look") (args)
      (declare (ignore args))
      "seen")
    agent))

(defun %two-calls (a b)
  (make-llm-response
   :parts (list (make-llm-tool-call-part :id "c1" :name a :arguments "{}")
                (make-llm-tool-call-part :id "c2" :name b :arguments "{}"))
   :finish-reason :tool-use))

(deftest tool-end-finishes-the-run
  (with-agent-loop
    (let* ((n (list 0))
           (run (run-ai-agent (%ender-agent n (list (%tool-response "c1" "propose")
                                                    (%text-response "never")))
                              "go")))
      (ok (eq :tool-end (agent-run-finish-reason run)))
      (ok (= 1 (car n)) "no model step follows")
      (ok (eq :tool (llm-turn-role (car (last (agent-run-turns run)))))
          "the conversation ends with the tool result")
      (ok (null (agent-invocation-end-run-p (first (agent-run-invocations run))))
          "the flag is spent"))))

(deftest tool-end-waits-for-the-whole-step
  (with-agent-loop
    (let* ((n (list 0))
           (run (run-ai-agent (%ender-agent n (list (%two-calls "propose" "look")
                                                    (%text-response "never")))
                              "go")))
      (ok (eq :tool-end (agent-run-finish-reason run)))
      (ok (= 1 (car n)))
      (ok (equal '("saved" "seen") (mapcar #'agent-invocation-result (agent-run-invocations run)))
          "the sibling call ran and is recorded"))))

(deftest tool-end-not-on-an-error
  (with-agent-loop
    (let* ((n (list 0))
           (run (run-ai-agent (%ender-agent n (list (%tool-response "c1" "propose")
                                                    (%text-response "fixed"))
                                            :fail t)
                              "go")))
      (ok (eq :stop (agent-run-finish-reason run)) "the model may retry")
      (ok (= 2 (car n))))))

(deftest tool-end-plain-output-continues
  (with-agent-loop
    (let* ((n (list 0))
           (run (run-ai-agent (%ender-agent n (list (%tool-response "c1" "look")
                                                    (%text-response "done")))
                              "go")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (= 2 (car n))))))

(deftest tool-end-replays
  (with-agent-loop
    (let* ((n (list 0))
           (agent (%ender-agent n (list (%tool-response "c1" "propose") (%text-response "never"))))
           (journal (task-protocol:make-in-memory-journal))
           (run (run-ai-agent agent "go" :durability (%dur journal "ender"))))
      (ok (eq :tool-end (agent-run-finish-reason run)))
      (let ((run2 (run-ai-agent agent "go"
                                :durability (%dur (task-protocol:copy-in-memory-journal journal)
                                                  "ender"))))
        (ok (eq :tool-end (agent-run-finish-reason run2)) "the journaled flag ends the replay")
        (ok (= 1 (car n)) "no generate on replay")))))
