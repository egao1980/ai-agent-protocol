(in-package #:ai-agent-protocol/tests)

;;; Lazily discovered, path-scoped instructions: tool calls that name a path
;;; pull in AGENTS.md files up to the root as a turn before the next generate,
;;; once per run, and again after a compaction.

(defun %instr-write (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(defmacro with-instr-project ((root) &body body)
  `(let ((,root (uiop:ensure-directory-pathname
                 (merge-pathnames (format nil "agent-instr-~a/" (random 1000000))
                                  (uiop:temporary-directory)))))
     (unwind-protect
          (progn
            (ensure-directories-exist ,root)
            (setf ,root (truename ,root))
            (%instr-write (merge-pathnames "AGENTS.md" ,root) "ROOT-RULES")
            (%instr-write (merge-pathnames "src/AGENTS.md" ,root) "SRC-RULES")
            (%instr-write (merge-pathnames "src/a.lisp" ,root) "(a)")
            ,@body)
       (uiop:delete-directory-tree ,root :validate t :if-does-not-exist :ignore))))

(defun %instruction-turns (turns)
  (remove-if-not (lambda (turn)
                   (and (eq :user (llm-turn-role turn))
                        (search "<instructions " (turn-text turn))))
                 turns))

(defun %scripted-backend (steps box)
  "STEPS: list of tool-call (name . arguments) or a string (final reply).
BOX's car collects the turns each generate saw, oldest first."
  (let ((n 0))
    (make-mock-llm-backend
     :handler (lambda (b turns &key &allow-other-keys)
                (declare (ignore b))
                (setf (car box) (append (car box) (list (copy-list turns))))
                (let ((step (nth (min n (1- (length steps))) steps)))
                  (incf n)
                  (if (stringp step)
                      (make-llm-response :parts (list (make-llm-text-part :text step))
                                         :finish-reason :stop)
                      (make-llm-response
                       :parts (list (make-llm-tool-call-part
                                     :id (format nil "c~d" n)
                                     :name (car step) :arguments (cdr step)))
                       :finish-reason :tool-use)))))))

(defun %instr-agent (backend &key extra steering)
  (let ((agent (make-ai-agent :name "coder" :backend backend
                              :steering steering
                              :settings (make-agent-settings :extra extra))))
    (define-agent-tool agent "read" () (args)
      (declare (ignore args))
      "contents")
    (define-agent-tool agent "noop" () (args)
      (declare (ignore args))
      "ok")
    agent))

(deftest instructions-injected-before-next-generate
  (with-agent-loop
    (with-instr-project (root)
      (let* ((box (list nil))
             (events (list nil))
             (agent (%instr-agent
                     (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}")
                                          ("read" . "{\"file_path\":\"src/b.lisp\"}")
                                          "done")
                                        box)
                     :extra (list :instructions-discovery root)))
             (run (run-ai-agent agent "go" :on-event (%events-into events))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (= 3 (length (car box))))
        (testing "first generate: nothing touched yet"
          (ok (null (%instruction-turns (first (car box))))))
        (testing "second generate: root + src instructions, after the tool result"
          (let* ((seen (second (car box)))
                 (instr (%instruction-turns seen)))
            (ok (= 1 (length instr)))
            (ok (eq (first instr) (car (last seen))))
            (ok (search "ROOT-RULES" (turn-text (first instr))))
            (ok (search "SRC-RULES" (turn-text (first instr))))
            (ok (search "<instructions path=\"src/AGENTS.md\" scope=\"src/\">"
                        (turn-text (first instr))))))
        (testing "third generate: same directory, not injected again"
          (ok (= 1 (length (%instruction-turns (third (car box)))))))
        (ok (= 1 (count :instructions (car events) :key #'car)))
        (ok (= 2 (length (steer-protocol:list-directives
                          (agent-run-instructions run)))))))))

(deftest instructions-off-without-config
  (with-agent-loop
    (with-instr-project (root)
      (let* ((box (list nil))
             (agent (%instr-agent
                     (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}") "done")
                                        box))))
        (run-ai-agent agent "go")
        (ok (null (%instruction-turns (car (last (car box))))))))))

(deftest instructions-via-steering-are-per-run
  (with-agent-loop
    (with-instr-project (root)
      (let* ((disc (steer-protocol:make-instruction-discovery root))
             (box (list nil))
             (agent (%instr-agent
                     (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}") "done")
                                        box)
                     :steering disc)))
        (run-ai-agent agent "go")
        (ok (= 1 (length (%instruction-turns (car (last (car box)))))))
        (testing "a second run gets the instructions again; the shared config is untouched"
          (setf (car box) nil)
          (let ((agent2 (%instr-agent
                         (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}") "done")
                                            box)
                         :steering disc)))
            (run-ai-agent agent2 "go")
            (ok (= 1 (length (%instruction-turns (car (last (car box)))))))
            (ok (null (steer-protocol:list-directives disc)))))))))

;;; Compactor that keeps only the last turn, once instructions are in context.
(defclass keep-last-compactor ()
  ((count :initform 0 :accessor keep-last-count)))

(defmethod conversation-protocol:compact-turns ((mem keep-last-compactor) turns
                                                &key backend)
  (declare (ignore backend))
  (if (and (zerop (keep-last-count mem)) (%instruction-turns turns))
      (progn
        (incf (keep-last-count mem))
        (values (list (user-turn "SUMMARY") (car (last turns))) t))
      (values turns nil)))

(deftest instructions-re-injected-after-compaction
  (with-agent-loop
    (with-instr-project (root)
      (let* ((box (list nil))
             (events (list nil))
             (mem (make-instance 'keep-last-compactor))
             (agent (%instr-agent
                     (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}")
                                          ("noop" . "{}")
                                          "done")
                                        box)
                     :extra (list :instructions-discovery root
                                  :compaction mem :compact-at 0))))
        (run-ai-agent agent "go" :on-event (%events-into events))
        (ok (= 1 (keep-last-count mem)))
        (ok (find :compacted (car events) :key #'car))
        (let ((seen (third (car box))))
          (testing "compaction dropped the first injection; it came back"
            (ok (equal "SUMMARY" (turn-text (first seen))))
            (ok (= 1 (length (%instruction-turns seen))))
            (ok (search "SRC-RULES" (turn-text (car (last seen)))))))
        (ok (= 2 (count :instructions (car events) :key #'car)))))))

(deftest instructions-change-re-injects
  (with-agent-loop
    (with-instr-project (root)
      (let* ((box (list nil))
             (backend (%scripted-backend '(("read" . "{\"path\": \"src/a.lisp\"}")
                                           ("edit" . "{\"path\": \"src/a.lisp\"}")
                                           "done")
                                         box))
             (agent (%instr-agent backend
                                  :extra (list :instructions-discovery root))))
        (define-agent-tool agent "edit" () (args)
          (declare (ignore args))
          (%instr-write (merge-pathnames "src/AGENTS.md" root) "SRC-RULES-V2")
          "edited")
        (run-ai-agent agent "go")
        (let ((instr (%instruction-turns (third (car box)))))
          (ok (= 2 (length instr)))
          (ok (search "SRC-RULES-V2" (turn-text (second instr))))
          (ng (search "ROOT-RULES" (turn-text (second instr)))))))))

(deftest tool-instruction-paths-defaults
  (ok (equal '("src/a.lisp")
             (tool-instruction-paths nil "read" "{\"path\": \"src/a.lisp\", \"limit\": 3}")))
  (ok (equal '("a" "b\"c")
             (tool-instruction-paths nil "x" "{\"paths\": [\"a\", \"b\\\"c\"], \"q\": \"z\"}")))
  (ok (null (tool-instruction-paths nil "x" "{\"command\": \"ls -la\"}")))
  (ok (equal '("p") (tool-instruction-paths nil "x" '(:file-path "p" :n 1))))
  (ok (null (tool-instruction-paths nil "x" "not json \"")))
  (testing "permission resources that look like paths count"
    (let ((tool (make-function-tool :name "e"
                                    :permission-resources
                                    (lambda (args)
                                      (declare (ignore args))
                                      (list "lib/x.lisp" "https://h/x" "rm -rf /")))))
      (ok (equal '("lib/x.lisp") (tool-instruction-paths tool "e" "{}"))))))
