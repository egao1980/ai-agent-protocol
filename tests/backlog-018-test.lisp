(in-package #:ai-agent-protocol/tests)

;;; 0.18.0: #47 (RETRY exported, COMPACT-AND-RETRY), #54 (sampling
;;; :ON-RESPONSE), #49 (wrapper sources delegate permission resources and
;;; instruction paths), #48 (:compacted carries the replaced turns), #46
;;; (manual compaction passes :instructions to COMPACT-TURNS).

;;; --- #47 -------------------------------------------------------------------

(deftest loop-retry-restart-is-exported
  (ok (eq :external (nth-value 1 (find-symbol "RETRY" :ai-agent-protocol))))
  (ok (eq 'llm-protocol:retry (find-symbol "RETRY" :ai-agent-protocol))
      "the same restart name as llm-protocol's")
  (ok (eq :external (nth-value 1 (find-symbol "COMPACT-AND-RETRY" :ai-agent-protocol)))))

(defun %overflow-until-summary-backend ()
  (make-mock-llm-backend
   :handler (lambda (b turns &key &allow-other-keys)
              (declare (ignore b))
              (if (find-if #'conversation-protocol:summary-turn-p turns)
                  (%resp "fits now")
                  (error 'llm-context-overflow :status 400 :message "prompt is too long")))))

(deftest compact-and-retry-restart
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
           (offered nil)
           (run (handler-bind ((agent-generate-error
                                 (lambda (c)
                                   (let ((r (find-restart 'compact-and-retry c)))
                                     (setf offered (and r t))
                                     (when r (invoke-restart r "Keep the file names."))))))
                  (run-ai-agent (make-ai-agent
                                 :name "x" :backend (%overflow-until-summary-backend)
                                 :settings (make-agent-settings
                                            :timeout 20 :extra (list :compaction mem)))
                                (%history) :on-event (%events-into events)))))
      (ok offered "a run with a compactor offers COMPACT-AND-RETRY")
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "fits now" (agent-run-text run)))
      (ok (search "Keep the file names." (or (first prompts) ""))
          "the restart's instructions reach the summarizer")
      (ok (eq :overflow (getf (cdr (find :compacted (car events) :key #'car)) :reason)))
      (ok (= 1 (count :compacted (car events) :key #'car)) "compacted once"))))

(deftest compact-and-retry-needs-a-compactor
  (with-agent-loop
    (let ((offered :unset))
      (ok (signals
           (handler-bind ((agent-generate-error
                            (lambda (c)
                              (setf offered (find-restart 'compact-and-retry c)))))
             (run-ai-agent (make-ai-agent :name "x" :backend (%overflow-until-summary-backend))
                           "hi"))
           'llm-context-overflow))
      (ok (null offered) "no compactor, no restart"))))

;;; --- #54 -------------------------------------------------------------------

(deftest sampling-handler-on-response
  (let* ((cell (list nil))
         (seen nil)
         (out (funcall (ai-agent-protocol/mcp:make-mcp-sampling-handler
                        :backend (%recording-backend cell)
                        :on-response (lambda (params response)
                                       (setf seen (list params response))))
                       (%sampling-params))))
    (ok (equal "sampled" (llm-response-text (second seen))) "the host gets the llm-response")
    (ok (hash-table-p (first seen)) "and the request params")
    (ok (gethash "content" out) "the result is unchanged")))

;;; --- #49 -------------------------------------------------------------------

(defun %resourced-tool (name)
  (make-function-tool :name name :description name
                      :permission-resources (lambda (args) (declare (ignore args))
                                              (list (format nil "skill(~a)" name) "src/x.lisp"))
                      :handler (lambda (args) (declare (ignore args)) name)))

(deftest wrappers-delegate-permission-resources-and-instruction-paths
  (let* ((inner (make-combined-tool-source (%resourced-tool "read")))
         (want '("skill(read)" "src/x.lisp")))
    (ok (equal want (tool-permission-resources inner "read" "{}"))
        "a combined source asks the tool that owns the name")
    (dolist (source (list (make-wrapper-tool-source inner)
                          (make-filtered-tool-source inner (constantly t))))
      (let ((what (string-downcase (symbol-name (type-of source)))))
        (ok (equal want (tool-permission-resources source "read" "{}"))
            (format nil "~A: permission resources" what))
        (ok (equal want (tool-permission-always-resources source "read" "{}"))
            (format nil "~A: always resources" what))
        (ok (member "src/x.lisp" (tool-instruction-paths source "read" "{}") :test #'equal)
            (format nil "~A: instruction paths" what))))
    (let ((prefixed (make-prefixed-tool-source inner "fs_"))
          (renamed (make-renamed-tool-source inner '(("cat" . "read")))))
      (ok (equal want (tool-permission-resources prefixed "fs_read" "{}"))
          "a prefixed source asks for the inner name")
      (ok (equal want (tool-permission-resources renamed "cat" "{}"))
          "a renamed source asks for the old name")
      (ok (equal '("*") (tool-permission-resources prefixed "read" "{}"))
          "a name the source does not offer gets the default"))))

;;; --- #48 -------------------------------------------------------------------

(deftest compacted-event-carries-the-replaced-turns
  (with-agent-loop
    (let* ((events (list nil))
           (mem (conversation-protocol:make-summary-memory :backend (%summarizer)
                                                           :tail-tokens 1))
           (history (%history)))
      (run-ai-agent (make-ai-agent :name "x" :backend (%overflow-until-summary-backend)
                                   :settings (make-agent-settings
                                              :timeout 20 :extra (list :compaction mem)))
                    history :on-event (%events-into events))
      (let* ((c (cdr (find :compacted (car events) :key #'car)))
             (before (getf c :turns-before))
             (replaced (getf c :replaced)))
        (ok (= (getf c :before) (length before)) ":turns-before is the history it compacted")
        (ok (= (- (getf c :before) (getf c :keep)) (length replaced)))
        (ok (every (lambda (turn) (member turn before :test #'eq)) replaced)
            ":replaced are the turns the summary stands for")
        (ok (equal "old question" (turn-text (first replaced))))))))

;;; --- #46 -------------------------------------------------------------------

(deftest manual-compaction-leaves-the-shared-prompt-alone
  (with-agent-loop
    (let* ((summarizer (make-mock-llm-backend
                        :handler (lambda (b turns &key &allow-other-keys)
                                   (declare (ignore b turns))
                                   (%resp "SUM"))))
           (mem (conversation-protocol:make-summary-memory :backend summarizer :tail-tokens 1))
           (prompt (conversation-protocol:summary-memory-prompt mem))
           (seen nil)
           (agent (make-ai-agent :name "x"
                                 :backend (make-mock-llm-backend
                                           :handler (%scripted (%resp "a1")))
                                 :settings (make-agent-settings :timeout 20
                                                                :extra (list :compaction mem))))
           (run (run-ai-agent agent (%history))))
      (handler-bind ((warning #'muffle-warning))
        (let ((observer (make-mock-llm-backend
                         :handler (lambda (b turns &key &allow-other-keys)
                                    (declare (ignore b turns))
                                    (setf seen (conversation-protocol:summary-memory-prompt mem))
                                    (%resp "SUM")))))
          (setf (conversation-protocol::summary-memory-backend mem) observer)))
      (compact-agent-run run :instructions "Keep the file names.")
      (event-protocol:run event-protocol:*event-backend* event-protocol:*event-loop*)
      (ok (eq prompt seen) "while it summarizes, the memory's prompt is its own")
      (ok (eq prompt (conversation-protocol:summary-memory-prompt mem))))))
