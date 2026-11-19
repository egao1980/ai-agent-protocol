(in-package #:ai-agent-protocol/tests)

;;; Durability across CONTINUE-AI-AGENT (#16) and compaction (#17). A "crash"
;;; is a journal copy taken mid-run; replay re-runs the same calls on it.

(defun %echo-tool-backend (counter)
  "A user turn → call tool \"noop\"; a tool result → reply with the user text
it answers. COUNTER's car counts generate calls."
  (flet ((asked-p (turn)
           (and (eq :user (llm-turn-role turn))
                (notany #'llm-tool-result-part-p (llm-turn-parts turn)))))
    (make-mock-llm-backend
     :handler (lambda (b turns &key &allow-other-keys)
                (declare (ignore b))
                (incf (car counter))
                (let ((last (%last-turn turns)))
                  (cond
                   ((find-if #'llm-tool-result-part-p (llm-turn-parts last))
                    (%say (format nil "answer: ~a"
                                  (turn-text (find-if #'asked-p turns :from-end t)))))
                   ((asked-p last)
                    (%call (format nil "c~d" (car counter)) "noop" "{}"))
                   (t (%say "?"))))))))

(defun %noop-agent (counter)
  (let ((agent (make-ai-agent :name "cont" :backend (%echo-tool-backend counter))))
    (define-agent-tool agent "noop" (:description "does nothing") (args)
      (declare (ignore args))
      "ok")
    agent))

(deftest durable-continue-keys-its-steps-apart
  (with-agent-loop
    (let* ((n (list 0))
           (agent (%noop-agent n))
           (journal (task-protocol:make-in-memory-journal))
           (in-continue nil)
           (crash nil)
           (run (run-ai-agent agent "first"
                              :durability (%dur journal "cont-run")
                              :on-event (lambda (kind payload)
                                          (when (and in-continue (eq kind :step)
                                                     (eql payload 2))
                                            (setf crash (task-protocol:copy-in-memory-journal
                                                         journal)))))))
      (ok (equal "answer: first" (agent-run-text run)))
      (ok (= 2 (car n)))
      (setf in-continue t)
      (continue-ai-agent run "second")
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "answer: second" (agent-run-text run))
          "the continuation generates; it does not replay the first segment")
      (ok (= 4 (car n)))
      (ok crash "journal copied inside the continuation")
      (testing "crash inside the continuation: replay finishes it"
        (let ((run2 (run-ai-agent agent "first" :durability (%dur crash "cont-run"))))
          (ok (= 4 (car n)) "the first segment replays")
          (continue-ai-agent run2 "second")
          (ok (= 5 (car n)) "only the generate after the crash runs")
          (ok (equal (%turn-texts run) (%turn-texts run2)) "same history")))
      (testing "full journal: both segments replay, nothing runs"
        (let ((run3 (run-ai-agent agent "first"
                                  :durability (%dur (task-protocol:copy-in-memory-journal
                                                     journal)
                                                    "cont-run"))))
          (continue-ai-agent run3 "second")
          (ok (= 5 (car n)))
          (ok (equal (%turn-texts run) (%turn-texts run3))))))))

(defun %counting-summarizer (counter)
  (make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b turns))
              (incf (car counter))
              (make-llm-response
               :parts (list (make-llm-text-part
                             :text (format nil "SUM-~d" (car counter))))))))

(deftest durable-compaction-replays-the-journaled-summary
  (with-agent-loop
    (let* ((n (list 0))
           (sn (list 0))
           (mem (conversation-protocol:make-summary-memory
                 :backend (%counting-summarizer sn) :tail-tokens 1))
           (agent (make-ai-agent :name "compact"
                                 :backend (%echo-tool-backend n)
                                 :settings (make-agent-settings
                                            :extra (list :compaction mem :compact-at 3))))
           (journal (task-protocol:make-in-memory-journal))
           (crash nil)
           (run (progn
                  (define-agent-tool agent "noop" (:description "does nothing") (args)
                    (declare (ignore args))
                    "ok")
                  (run-ai-agent agent (%history)
                                :durability (%dur journal "compact-run")
                                :on-event (lambda (kind payload)
                                            (when (and (eq kind :step) (eql payload 2))
                                              (setf crash (task-protocol:copy-in-memory-journal
                                                           journal))))))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (plusp (car sn)) "the run compacted")
      (ok crash)
      (let ((summaries (car sn)))
        (testing "crash after a compaction: the summary replays"
          (let ((run2 (run-ai-agent agent (%history)
                                    :durability (%dur crash "compact-run"))))
            (ok (eq :stop (agent-run-finish-reason run2)))
            (ok (equal (%turn-texts run) (%turn-texts run2)) "same history")
            (ok (= summaries (car sn)) "no summarizer call")))
        (testing "full journal: no summarizer call"
          (let* ((before (car sn))
                 (run3 (run-ai-agent agent (%history)
                                     :durability (%dur (task-protocol:copy-in-memory-journal
                                                        journal)
                                                       "compact-run"))))
            (ok (= before (car sn)))
            (ok (equal (%turn-texts run) (%turn-texts run3)))))))))
