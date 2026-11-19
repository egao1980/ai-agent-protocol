(in-package #:ai-agent-protocol/tests)

;;; Durability of long-run state (0.7): background subagents, the inbox and
;;; the injected instruction set. A "crash" is a copy of the journal taken
;;; mid-run; replay is RUN-AI-AGENT on that copy. Children that were running
;;; at the crash cannot be reattached: they are marked :failed and the parent
;;; gets a status="failed" report.

(defun %dur (journal id)
  (ai-agent-protocol/durability:make-agent-durability :journal journal :task-id id))

(defun %turn-texts (run)
  (mapcar (lambda (turn) (list (llm-turn-role turn) (turn-text turn)))
          (agent-run-turns run)))

(defun %counting-echo (counter &optional (delay 0.05))
  "Child backend: echoes the last user text; COUNTER's car counts calls."
  (make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b))
              (incf (car counter))
              (sleep delay)
              (%say (format nil "echo: ~a" (turn-text (%last-turn turns)))))))

(defun %lead (helper-counter parent-counter)
  (let ((helper (make-ai-agent :name "helper" :backend (%counting-echo helper-counter))))
    (make-ai-agent :name "lead"
                   :backend (%parent-backend
                             (lambda (turns reports)
                               (incf (car parent-counter))
                               (%one-spawn-plan turns reports)))
                   :tools (list (make-subagent-tool (list helper))))))

(deftest durable-subagent-report-and-inbox-replay-exactly
  (with-agent-loop
    (let* ((hn (list 0)) (pn (list 0))
           (agent (%lead hn pn))
           (journal (task-protocol:make-in-memory-journal))
           (crash nil)
           (run (run-ai-agent agent "go"
                              :durability (%dur journal "sub-run")
                              :on-event (lambda (kind payload)
                                          ;; Step 3 starts with the report
                                          ;; queued but not yet delivered.
                                          (when (and (eq kind :step) (eql payload 3))
                                            (setf crash (task-protocol:copy-in-memory-journal
                                                         journal)))))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (= 1 (car hn)))
      (ok (= 3 (car pn)))
      (ok crash)
      (testing "crash with an undelivered report: it is delivered, not lost"
        (let ((run2 (run-ai-agent agent "go" :durability (%dur crash "sub-run"))))
          (ok (eq :stop (agent-run-finish-reason run2)))
          (ok (= 1 (car hn)) "the child does not run again")
          (ok (= 4 (car pn)) "only the undone generate runs")
          (ok (equal (%turn-texts run) (%turn-texts run2)) "same history")
          (ok (equal (agent-run-text run) (agent-run-text run2)))
          (let ((task (find-subagent-task run2 "sub-1")))
            (ok task "the child's task is restored")
            (ok (eq :idle (subagent-task-status task)))
            (ok (equal "echo: dig" (subagent-task-result task)))
            (ok (equal "echo: dig" (turn-text (%last-turn
                                                (agent-run-turns (subagent-task-run task)))))
                "an idle child's history is restored (it can be continued)"))))
      (testing "full journal: everything replays, nothing runs"
        (let ((run3 (run-ai-agent agent "go" :durability
                                  (%dur (task-protocol:copy-in-memory-journal journal)
                                        "sub-run"))))
          (ok (eq :stop (agent-run-finish-reason run3)))
          (ok (= 1 (car hn)))
          (ok (= 4 (car pn)))
          (ok (equal (%turn-texts run) (%turn-texts run3)))
          (ok (eq :idle (subagent-task-status (find-subagent-task run3 "sub-1")))))))))

(deftest durable-subagent-live-at-crash-is-reported-failed
  (with-agent-loop
    (let* ((hn (list 0)) (pn (list 0))
           (agent (%lead hn pn))
           (journal (task-protocol:make-in-memory-journal))
           (crash nil))
      (run-ai-agent agent "go"
                    :durability (%dur journal "lost-run")
                    :on-event (lambda (kind payload)
                                (declare (ignore payload))
                                (when (and (eq kind :waiting) (null crash))
                                  (setf crash (task-protocol:copy-in-memory-journal
                                               journal)))))
      (ok crash "journal copied while the child was running")
      (let* ((events (list nil))
             (run2 (run-ai-agent agent "go" :durability (%dur crash "lost-run")
                                            :on-event (%events-into events)))
             (task (find-subagent-task run2 "sub-1")))
        (ok (eq :stop (agent-run-finish-reason run2)))
        (ok (= 1 (car hn)) "the lost child is not restarted")
        (ok (eq :failed (subagent-task-status task)))
        (ok (search "<subagent id=\"sub-1\" name=\"helper\" status=\"failed\">"
                    (agent-run-text run2))
            "the parent got a failure report and went on")
        (ok (search "lost" (subagent-task-result task)))
        (ok (find-if (lambda (e) (and (eq :subagent (car e))
                                      (eq :lost (getf (cdr e) :event))))
                     (car events)))
        (testing "a second crash replays the failure report too"
          (let* ((n-before (car pn))
                 (run3 (run-ai-agent agent "go"
                                     :durability
                                     (%dur (task-protocol:copy-in-memory-journal
                                            (ai-agent-protocol/durability:agent-durability-journal
                                             (agent-run-durability run2)))
                                           "lost-run"))))
            (ok (= n-before (car pn)) "nothing new runs")
            (ok (equal (%turn-texts run2) (%turn-texts run3)))
            (ok (eq :failed (subagent-task-status (find-subagent-task run3 "sub-1"))))))))))

(deftest durable-instructions-round-trip
  (with-agent-loop
    (with-instr-project (root)
      (let* ((box (list nil))
             (agent (%instr-agent
                     (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}")
                                          ("read" . "{\"path\": \"src/b.lisp\"}")
                                          "done")
                                        box)
                     :extra (list :instructions-discovery root)))
             (journal (task-protocol:make-in-memory-journal))
             (crash nil)
             (run (run-ai-agent agent "go"
                                :durability (%dur journal "instr-run")
                                :on-event (lambda (kind payload)
                                            (when (and (eq kind :step) (eql payload 3))
                                              (setf crash (task-protocol:copy-in-memory-journal
                                                           journal)))))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (= 1 (length (%instruction-turns (agent-run-turns run)))))
        (testing "crash after injection: the set is restored, not re-injected"
          (let ((run2 (run-ai-agent agent "go" :durability (%dur crash "instr-run"))))
            (ok (eq :stop (agent-run-finish-reason run2)))
            (ok (= 1 (length (%instruction-turns (agent-run-turns run2)))))
            (ok (equal (%turn-texts run) (%turn-texts run2)))
            (ok (= 2 (length (steer-protocol:list-directives (agent-run-instructions run2)))))))
        (testing "replay injects what was journaled, not what is on disk now"
          (%instr-write (merge-pathnames "AGENTS.md" root) "ROOT-CHANGED")
          (let ((run3 (run-ai-agent agent "go"
                                    :durability (%dur (task-protocol:copy-in-memory-journal
                                                       journal)
                                                      "instr-run"))))
            (ok (equal (%turn-texts run) (%turn-texts run3)))
            (ok (search "ROOT-RULES" (turn-text (first (%instruction-turns
                                                         (agent-run-turns run3))))))))))))

(defparameter +state-step-names+ '("agent-state" "inbox" "instructions"))

(defun %legacy-copy (journal task-id)
  "JOURNAL as a pre-0.7 journal: without the run-state steps."
  (task-protocol:make-in-memory-journal
   :events (mapcar #'task-protocol:copy-event
                   (remove-if (lambda (e)
                                (and (typep e 'task-protocol:step-completed)
                                     (member (task-protocol:step-name e) +state-step-names+
                                             :test #'equal)))
                              (task-protocol:journal-events journal task-id)))))

(deftest durable-legacy-journal-still-replays
  (with-agent-loop
    (with-instr-project (root)
      (let* ((box (list nil))
             (agent (%instr-agent
                     (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}") "done") box)
                     :extra (list :instructions-discovery root)))
             (journal (task-protocol:make-in-memory-journal))
             (run (run-ai-agent agent "go" :durability (%dur journal "old-run")))
             (old (%legacy-copy journal "old-run")))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (find "agent-state" (task-protocol:journal-events journal "old-run")
                  :key (lambda (e) (and (typep e 'task-protocol:step-completed)
                                        (task-protocol:step-name e)))
                  :test #'equal)
            "new journals carry the state marker")
        (let ((run2 (run-ai-agent agent "go" :durability (%dur old "old-run"))))
          (ok (eq :stop (agent-run-finish-reason run2)))
          (ok (= 2 (length (car box))) "replay calls no generate")
          (ok (equal (%turn-texts run) (%turn-texts run2))
              "instructions are found again from disk, as before"))))))
