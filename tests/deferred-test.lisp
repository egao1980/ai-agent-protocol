(in-package #:ai-agent-protocol/tests)

;;; 0.17.0 (H3e-2b): deferred tool loading (src/deferred.lisp). Function tools
;;; in a combined source stand for a large inner source (an MCP server).

(defparameter +df-topics+ '("weather" "calendar" "invoice" "github" "database"))

(defun %df-tools (n)
  "N function tools tool_01 … tool_NN, \"Extra tool NN about TOPIC\"; result \"NAME ran\"."
  (loop for k from 1 to n
        collect (let ((name (format nil "tool_~2,'0d" k)))
                  (make-function-tool :name name
                                      :description (format nil "Extra tool ~2,'0d about ~a" k
                                                           (nth (mod (1- k) 5) +df-topics+))
                                      :handler (lambda (args) (declare (ignore args))
                                                 (format nil "~a ran" name))))))

(defun %df (n &rest keys)
  (apply #'make-deferred-tool-source (apply #'make-combined-tool-source (%df-tools n)) keys))

(defun %df-search (source query &optional max-results)
  (%ts-call source "tool_search"
            (if max-results
                (format nil "{\"query\":~s,\"max_results\":~d}" query max-results)
                (format nil "{\"query\":~s}" query))))

(deftest deferred-inactive-below-threshold
  (let ((d (%df 3 :threshold 5)))
    (ok (equal '("tool_01" "tool_02" "tool_03") (%ts-names d)) "the inner listing")
    (ok (not (deferred-tool-source-active-p d)))
    (ok (null (deferred-tool-source-deferred d)))
    (ok (not (tool-executable-p d "tool_search")) "no search tool")
    (ok (equal "tool_02 ran" (%ts-call d "tool_02")))))

(deftest deferred-active-above-threshold
  (let ((d (%df 6 :threshold 5)))
    (ok (deferred-tool-source-active-p d))
    (ok (equal '("tool_search") (%ts-names d)) "only the search tool")
    (ok (equal (mapcar #'llm-tool-name (%df-tools 6)) (deferred-tool-source-deferred d)))
    (ok (tool-executable-p d "tool_search"))
    (ok (tool-executable-p d "tool_03") "a deferred tool can be called by name (auto-load)")
    (ok (not (tool-executable-p d "tool_99")) "a name the inner source lacks cannot")
    (ok (not (tool-needs-approval-p d "tool_search" "{\"query\":\"x\"}")) "never asks")
    (ok (equal '("*") (tool-permission-resources d "tool_search" "{\"query\":\"x\"}")))))

(deftest-parametrize deferred-modes
    ((mode threshold n active)
     (:auto 5 5 nil)
     (:auto 5 6 t)
     (:auto 0 1 t)
     (:always 40 1 t)
     (:always 40 0 nil)
     (:never 0 50 nil))
  (ok (eq active (and (deferred-tool-source-active-p (%df n :mode mode :threshold threshold)) t))))

(deftest deferred-keep
  (let ((d (%df 8 :threshold 5 :keep (lambda (context tool) (declare (ignore context))
                                       (member (llm-tool-name tool) '("tool_02" "tool_05") :test #'equal)))))
    (ok (deferred-tool-source-active-p d) "6 deferrable > 5")
    (ok (equal '("tool_02" "tool_05" "tool_search") (%ts-names d)) "kept tools stay, in order")
    (ok (equal "tool_05 ran" (%ts-call d "tool_05"))))
  (let ((d (%df 6 :threshold 5 :keep (lambda (context tool) (declare (ignore context))
                                       (equal "tool_01" (llm-tool-name tool))))))
    (ok (not (deferred-tool-source-active-p d)) "kept tools do not count")))

(deftest deferred-search-tool-definition
  (let* ((d (%df 6 :threshold 5))
         (tool (find "tool_search" (list-agent-tools d) :key #'llm-tool-name :test #'equal))
         (description (llm-tool-description tool)))
    (ok (uiop:string-prefix-p +tool-search-description+ description))
    (ok (search "Deferred tools: tool_01, tool_02, tool_03, tool_04, tool_05, tool_06" description))
    (let ((schema (llm-tool-parameters tool)))
      (ok (equal "object" (gethash "type" schema)))
      (ok (equal '("query") (coerce (gethash "required" schema) 'list))))))

(deftest deferred-search-loads
  (let* ((loads '())
         (d (%df 12 :threshold 5 :on-load (lambda (names &key via) (push (list names via) loads))))
         (text (%df-search d "invoice")))
    (ok (search "Loaded 2 tools; they can be called from the next step:" text))
    (ok (search "- tool_03: Extra tool 03 about invoice" text))
    (ok (search "- tool_08: Extra tool 08 about invoice" text))
    (ok (equal '((("tool_03" "tool_08") :search)) loads) "ON-LOAD once, with the new names, via :search")
    (ok (equal '("tool_03" "tool_08" "tool_search") (%ts-names d)) "offered from now on, inner order")
    (ok (equal "tool_08 ran" (%ts-call d "tool_08")))
    (ok (equal '("tool_03" "tool_08") (deferred-tool-source-loaded d)))
    (%df-search d "invoice")
    (ok (= 1 (length loads)) "nothing new: no ON-LOAD")))

(deftest deferred-search-select
  (let* ((d (%df 12 :threshold 5))
         (text (%df-search d "select:tool_11,nope,tool_02")))
    (ok (search "Loaded 2 tools" text))
    (ok (search "Not found: nope" text))
    (ok (equal '("tool_02" "tool_11" "tool_search") (%ts-names d)))))

(deftest deferred-search-no-match-and-errors
  (let ((d (%df 12 :threshold 5)))
    (let ((text (%df-search d "spaceship")))
      (ok (search "No deferred tool matches \"spaceship\" (12 deferred)" text)))
    (ok (typep (nth-value 1 (%ts-call d "tool_search" "{}")) 'agent-error) "a query is needed")
    (ok (typep (nth-value 1 (%df-search d "invoice" 50)) 'agent-error) "max_results 1..20")
    (ok (equal '("tool_search") (%ts-names d)) "nothing loaded")))

(deftest deferred-search-max-results
  (let ((d (%df 12 :threshold 5)))
    (%df-search d "extra" 3)
    (ok (= 3 (length (deferred-tool-source-loaded d))) "every tool matches \"extra\": the first 3")))

(deftest deferred-call-loads
  (let* ((loads '())
         (d (%df 6 :threshold 5 :on-load (lambda (names &key via) (push (list names via) loads)))))
    (ok (equal "tool_04 ran" (%ts-call d "tool_04")) "called by name before any search: it runs")
    (ok (equal '((("tool_04") :call)) loads) "loaded via :call")
    (ok (equal '("tool_04") (deferred-tool-source-loaded d)))
    (ok (equal '("tool_04" "tool_search") (%ts-names d)) "listed from the next step on")
    (%ts-call d "tool_04")
    (ok (= 1 (length loads)) "a loaded tool's call loads nothing")
    (multiple-value-bind (result e) (%ts-call d "tool_99")
      (ok (null result))
      (ok (typep e 'agent-unknown-tool-error) "a name the inner source lacks stays unknown")
      (ok (equal '("tool_04") (deferred-tool-source-loaded d))))))

(deftest deferred-call-keeps-the-permission-path
  (let ((d (make-deferred-tool-source
            (make-combined-tool-source
             (%ts-ft "a") (%ts-ft "b") (%ts-ft "c") (%ts-ft "guarded" "G" :approval t))
            :threshold 2)))
    (ok (deferred-tool-source-active-p d))
    (ok (tool-needs-approval-p d "guarded" "{}") "the inner source's approval, before any load")
    (ok (not (tool-needs-approval-p d "a" "{}")))
    (ok (null (deferred-tool-source-loaded d)) "asking about a call loads nothing")))

(deftest deferred-call-through-the-loop
  (with-agent-loop
    (let* ((d (%df 6 :threshold 5))
           (backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part :id "c1" :name "tool_05" :arguments "{}"))
                               "done")))
           (agent (make-ai-agent :name "deferred" :backend backend :tools (list d)))
           (run (run-ai-agent agent "go")))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "tool_05 ran" (agent-invocation-result (first (agent-run-invocations run))))
          "the loop routes a name no source lists to the source that executes it")
      (ok (equal '("tool_05") (deferred-tool-source-loaded d))))))

(deftest deferred-loaded-setf
  (let ((d (%df 6 :threshold 5)))
    (setf (deferred-tool-source-loaded d) '("tool_06" "tool_01" "gone"))
    (ok (equal '("tool_01" "tool_06" "tool_search") (%ts-names d)) "inner order; a missing name skipped")
    (ok (equal '("tool_06" "tool_01" "gone") (deferred-tool-source-loaded d)) "and kept")
    (ok (equal '("tool_02" "tool_03" "tool_04" "tool_05") (deferred-tool-source-deferred d)))))

(deftest-parametrize tool-search-ranking
    ((query expected)
     ("invoice" '("tool_03" "tool_08"))
     ("tool_07" '("tool_07"))
     ("github invoice" '("tool_03" "tool_04" "tool_08" "tool_09"))
     ("INVOICE, weather!" '("tool_01" "tool_03" "tool_06" "tool_08"))
     ("  " ()))
  (ok (equal expected (mapcar #'llm-tool-name (tool-search-matches (%df-tools 10) query :max-results 10)))))

(deftest tool-search-name-weighs-more
  (let ((tools (list (make-function-tool :name "notes_read" :description "read a file"
                                         :handler (constantly "x"))
                     (make-function-tool :name "fs_read" :description "notes"
                                         :handler (constantly "y")))))
    (ok (equal '("notes_read" "fs_read") (mapcar #'llm-tool-name (tool-search-matches tools "notes")))
        "a name hit (2) before a description hit (1)")
    (multiple-value-bind (matches missing) (tool-search-matches tools "select:fs_read,zz")
      (ok (equal '("fs_read") (mapcar #'llm-tool-name matches)))
      (ok (equal '("zz") missing)))))
