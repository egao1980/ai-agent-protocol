(in-package #:ai-agent-protocol)

;;; 0.17.0 (contract, egao1980/ai-agent-protocol H3e-2b issue; after 0.16.0,
;;; #39 / #40): deferred tool loading. Prior art: pydantic-ai's deferred
;;; loading behind a search tool, Claude Code's ToolSearch ("select:A,B" and
;;; keyword queries). Found while contracting cl-stack-code's MCP part 2
;;; (H3e-2b, brief: cl-stack docs/capabilities/cl-stack-code.md "H3e-2b —
;;; locked"; this library's brief: docs/capabilities/ai-agent.md "0.17.0").
;;; GIVEN: the class, its constructor and readers, the constants. STUBBED:
;;; every method and function below; the loop's routing change (below) is the
;;; implementation's. Nothing in it is MCP-specific: any source can be
;;; deferred. Amended 2026-11-23 (the user's decision): a deferred tool called
;;; by name before any search is loaded and run, not refused, because a skill
;;; or instructions can name it directly.
;;;
;;; DEFERRED-TOOL-SOURCE (MAKE-DEFERRED-TOOL-SOURCE INNER &key (MODE :AUTO)
;;;   (THRESHOLD 40) KEEP (SEARCH-TOOL-NAME +TOOL-SEARCH-NAME+)
;;;   (MAX-RESULTS +TOOL-SEARCH-DEFAULT-RESULTS+) ON-LOAD), a WRAPPER-TOOL-SOURCE.
;;;   Deferrable tools: INNER's listing (with the CONTEXT given) except those
;;;   KEEP holds for ((TOOL-PREDICATE-P KEEP CONTEXT TOOL): a function or a
;;;   policy; NIL keeps none). Deferral is ACTIVE when MODE is :ALWAYS and at
;;;   least one tool is deferrable, or :AUTO and more than THRESHOLD are; never
;;;   for :NEVER. DEFERRED-TOOL-SOURCE-ACTIVE-P source &key context.
;;;   LIST-AGENT-TOOLS: inactive → INNER's listing. Active → INNER's listing in
;;;   its order without the deferrable tools that are not loaded, then the
;;;   search tool, last: an LLM-TOOL named SEARCH-TOOL-NAME whose description
;;;   is +TOOL-SEARCH-DESCRIPTION+, LF, "Deferred tools: " and the deferred
;;;   names not loaded, comma-separated (the first 200, then "… and N more"),
;;;   and whose parameters are {"type": "object", "properties": {"query":
;;;   {"type": "string"}, "max_results": {"type": "integer", "minimum": 1,
;;;   "maximum": 20}}, "required": ["query"]}.
;;;   DEFERRED-TOOL-SOURCE-DEFERRED source &key context → the deferrable names
;;;   not loaded, in INNER's order (NIL while inactive).
;;;   The search tool: TOOL-EXECUTABLE-P T while active (NIL otherwise: an
;;;   unknown tool); TOOL-NEEDS-APPROVAL-P NIL; TOOL-PERMISSION-RESOURCES
;;;   ("*"). INVOKE-TOOL-ASYNC with arguments (JSON text, hash table or plist,
;;;   as other sources take them) {"query", "max_results"}: matches =
;;;   TOOL-SEARCH-MATCHES of the deferrable tools; each is added to the loaded
;;;   set; ON-LOAD (a function of the list of names newly loaded, in INNER's
;;;   order, and :VIA :SEARCH) is called when there are any, before the
;;;   callback. The result
;;;   text:
;;;     "Loaded N tool[s]; they can be called from the next step:" then one
;;;     line "- NAME: DESCRIPTION" per match (the description's first line,
;;;     at most 200 characters), then for select: names not found one line
;;;     "Not found: NAME, NAME";
;;;     no match: "No deferred tool matches \"QUERY\" (N deferred). Try other
;;;     words or select:NAME."
;;;   A missing or non-string query, or max_results outside 1..20 → a tool
;;;   error (ERROR-CALLBACK, AGENT-ERROR "tool_search needs a query" / "…
;;;   max_results must be 1 to 20"), the model reads it.
;;;   A deferrable tool not loaded (while active), called by its exact name
;;;   (auto-load): TOOL-EXECUTABLE-P T and TOOL-NEEDS-APPROVAL-P INNER's (the
;;;   call goes through the host's permission path as any call: rules,
;;;   approvals, plan mode). INVOKE-TOOL-ASYNC (so only a call that the
;;;   permission path let run) first adds NAME to the loaded set and calls
;;;   ON-LOAD with (NAME) and :VIA :CALL, then delegates to INNER; from the
;;;   next step on the tool is listed. A denied or rejected call loads
;;;   nothing. A name INNER does not offer stays an unknown tool (INNER's
;;;   AGENT-UNKNOWN-TOOL-ERROR). Every other name delegates to INNER (as a
;;;   wrapper).
;;;   The loop (src/loop.lisp, %FIND-SOURCE): a call whose name no source
;;;   lists goes to the first source (in the run's order) whose
;;;   TOOL-EXECUTABLE-P is true for it, so a tool can be called before it is
;;;   listed; a name no source lists or executes is unknown, as before.
;;;   The loaded set: DEFERRED-TOOL-SOURCE-LOADED source → names in load
;;;   order; (SETF DEFERRED-TOOL-SOURCE-LOADED) replaces it (a host restoring
;;;   a revision). A loaded name INNER no longer lists is skipped in listings
;;;   and kept in the set. It lives as long as the source (a host that keeps
;;;   one source per session keeps the set across runs). Reads and writes are
;;;   under the source's lock (listings run on any thread).
;;;
;;; TOOL-SEARCH-MATCHES tools query &key (max-results +TOOL-SEARCH-DEFAULT-RESULTS+)
;;;   → the matching LLM-TOOLs, at most MAX-RESULTS:
;;;   QUERY (trimmed) "select:A,B" → the tools named A, B (exact, in TOOLS'
;;;   order; MAX-RESULTS does not apply), and as a second value the names not
;;;   found, in QUERY's order;
;;;   else the words of QUERY (split at whitespace and at punctuation other
;;;   than _ and -, lower case, empty ones dropped): a tool's score is, per
;;;   word, 2 when the word occurs in its name (case-insensitive), else 1 when
;;;   in its description; tools scoring 0 are dropped, the rest ordered by
;;;   score (higher first), then TOOLS' order. No READ, no regex engine.
;;;
;;; ON-LOAD: (lambda (names &key via)), VIA :SEARCH or :CALL.
;;;
;;; Non-goals: embeddings or BM25 ranking (a host may wrap the source and rank
;;; its own way), deferring per run (the set is the source's), unloading.

(defparameter +tool-search-name+ "tool_search")

(defparameter +tool-search-default-results+ 5)

(defparameter +tool-search-description+
  "Search the deferred tools and load the matches: they can be called from the next step. Query with words from a tool's name or description, or select:NAME,NAME for exact names.")

(defclass deferred-tool-source (wrapper-tool-source)
  ((mode :initarg :mode :initform :auto :reader deferred-tool-source-mode)
   (threshold :initarg :threshold :initform 40 :reader deferred-tool-source-threshold)
   (keep :initarg :keep :initform nil :reader deferred-tool-source-keep)
   (search-tool-name :initarg :search-tool-name :initform +tool-search-name+
                     :reader deferred-tool-source-search-tool-name)
   (max-results :initarg :max-results :initform +tool-search-default-results+
                :reader deferred-tool-source-max-results)
   (on-load :initarg :on-load :initform nil :reader deferred-tool-source-on-load)
   (loaded :initform '() :accessor %deferred-loaded)
   (lock :initform (bt:make-lock "deferred-tool-source") :reader %deferred-lock))
  (:documentation "0.17.0: INNER's tools behind a search tool (see the header)."))

(defun make-deferred-tool-source (inner &key (mode :auto) (threshold 40) keep
                                          (search-tool-name +tool-search-name+)
                                          (max-results +tool-search-default-results+) on-load)
  (check-type mode (member :auto :always :never))
  (check-type threshold (integer 0))
  (make-instance 'deferred-tool-source :inner inner :mode mode :threshold threshold :keep keep
                                       :search-tool-name search-tool-name :max-results max-results
                                       :on-load on-load))

(defun %deferred-json (&rest kvs)
  "A fresh EQUAL hash table of the KEY VALUE pairs KVS (subagents.lisp's
%JSON-OBJECT loads after this file)."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr do (setf (gethash k h) v))
    h))

(defun deferred-tool-source-loaded (source)
  "→ the loaded names, in load order."
  (bt:with-lock-held ((%deferred-lock source))
    (copy-list (%deferred-loaded source))))

(defun (setf deferred-tool-source-loaded) (names source)
  (check-type names list)
  (let ((copy (remove-duplicates (copy-list names) :test #'equal :from-end t)))
    (bt:with-lock-held ((%deferred-lock source))
      (setf (%deferred-loaded source) copy)))
  names)

(defun %deferred-add-loaded (source names)
  "Append the NAMES not loaded yet (NAMES' order) to the loaded set. → the new
ones. Holds the lock only for the update: no callouts."
  (bt:with-lock-held ((%deferred-lock source))
    (let* ((old (%deferred-loaded source))
           (new (remove-if (lambda (name) (member name old :test #'equal))
                           (remove-duplicates names :test #'equal :from-end t))))
      (when new
        (setf (%deferred-loaded source) (append old new)))
      new)))

(defun %deferred-notify (source names via)
  "ON-LOAD with NAMES and VIA, when there is an ON-LOAD and NAMES. Outside the lock."
  (let ((fn (deferred-tool-source-on-load source)))
    (when (and fn names)
      (funcall fn names :via via))))

(defun %deferral (source context)
  "One snapshot of INNER for CONTEXT → (values tools deferrable active-p).
INNER is listed outside the lock."
  (let* ((tools (list-agent-tools (tool-source-inner source) :context context))
         (keep (deferred-tool-source-keep source))
         (deferrable (if keep
                         (remove-if (lambda (tool) (tool-predicate-p keep context tool)) tools)
                         tools))
         (n (length deferrable)))
    (values tools
            deferrable
            (ecase (deferred-tool-source-mode source)
              (:never nil)
              (:always (plusp n))
              (:auto (> n (deferred-tool-source-threshold source)))))))

(defun %deferred-pending-names (source deferrable)
  "The names of DEFERRABLE not loaded, in DEFERRABLE's order."
  (let ((loaded (deferred-tool-source-loaded source)))
    (loop for tool in deferrable
          for name = (llm-tool-name tool)
          unless (member name loaded :test #'equal)
            collect name)))

(defun deferred-tool-source-active-p (source &key context)
  (nth-value 2 (%deferral source context)))

(defun deferred-tool-source-deferred (source &key context)
  (multiple-value-bind (tools deferrable active) (%deferral source context)
    (declare (ignore tools))
    (and active (%deferred-pending-names source deferrable))))

(defun %deferred-trim (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline #\Page) string))

(defun %query-word-char-p (char)
  (or (alphanumericp char) (char= char #\_) (char= char #\-)))

(defun %query-words (query)
  "QUERY's words, lower case: split at whitespace and punctuation other than _ and -."
  (let ((words '())
        (start nil)
        (n (length query)))
    (loop for i from 0 to n
          for char = (and (< i n) (char query i))
          do (cond ((and char (%query-word-char-p char))
                    (unless start (setf start i)))
                   (start
                    (push (string-downcase (subseq query start i)) words)
                    (setf start nil))))
    (nreverse words)))

(defun %select-names (text)
  "The comma-separated names of TEXT, trimmed, empty ones and repeats dropped."
  (let ((names '())
        (start 0))
    (loop for comma = (position #\, text :start start)
          for name = (%deferred-trim (subseq text start (or comma (length text))))
          do (when (and (plusp (length name)) (not (member name names :test #'string=)))
               (push name names))
             (if comma (setf start (1+ comma)) (loop-finish)))
    (nreverse names)))

(defun tool-search-matches (tools query &key (max-results +tool-search-default-results+))
  (let ((q (%deferred-trim query)))
    (if (and (>= (length q) 7) (string-equal "select:" q :end2 7))
        (let* ((wanted (%select-names (subseq q 7)))
               (found (remove-if-not (lambda (tool) (member (llm-tool-name tool) wanted :test #'equal))
                                     tools)))
          (values found
                  (remove-if (lambda (name) (%find-tool name found)) wanted)))
        (let* ((words (%query-words q))
               (scored (loop for tool in tools
                             for name = (llm-tool-name tool)
                             for description = (or (llm-tool-description tool) "")
                             for score = (loop for word in words
                                               sum (cond ((search word name :test #'char-equal) 2)
                                                         ((search word description :test #'char-equal) 1)
                                                         (t 0)))
                             when (plusp score)
                               collect (cons score tool)))
               (sorted (mapcar #'cdr (stable-sort scored #'> :key #'car))))
          (values (subseq sorted 0 (min (length sorted) max-results)) nil)))))

(defun %search-name-p (source name)
  (equal name (deferred-tool-source-search-tool-name source)))

(defun %search-tool (source pending)
  "The search tool's LLM-TOOL; PENDING: the deferred names not loaded."
  (let ((n (length pending)))
    (make-llm-tool
     :name (deferred-tool-source-search-tool-name source)
     :description (format nil "~a~%Deferred tools: ~{~a~^, ~}~@[, … and ~d more~]"
                          +tool-search-description+
                          (subseq pending 0 (min n 200))
                          (and (> n 200) (- n 200)))
     :parameters (%deferred-json
                  "type" "object"
                  "properties" (%deferred-json
                                "query" (%deferred-json "type" "string")
                                "max_results" (%deferred-json "type" "integer"
                                                              "minimum" 1 "maximum" 20))
                  "required" (vector "query")))))

(defun %search-arguments (source arguments)
  "ARGUMENTS (JSON text, hash table or plist) → (values query max-results).
Signals AGENT-ERROR on a missing or non-string query, or max_results outside 1..20."
  (let* ((tool (deferred-tool-source-search-tool-name source))
         (args (cond ((hash-table-p arguments) arguments)
                     ((stringp arguments)
                      (let ((parsed (try-parse-json-output arguments)))
                        (and (hash-table-p parsed) parsed)))
                     ((and (consp arguments) (keywordp (car arguments)))
                      (%deferred-json "query" (getf arguments :query)
                                      "max_results" (or (getf arguments :max-results)
                                                        (getf arguments :max_results))))))
         (query (and args (gethash "query" args)))
         (max (and args (gethash "max_results" args))))
    (unless (stringp query)
      (error 'agent-error :message (format nil "~a needs a query" tool)))
    (when (or (null max) (eq max :null) (eq max 'null))
      (setf max (deferred-tool-source-max-results source)))
    (unless (typep max '(integer 1 20))
      (error 'agent-error :message (format nil "~a max_results must be 1 to 20" tool)))
    (values query max)))

(defun %first-line (text)
  (let* ((text (or text ""))
         (line (subseq text 0 (or (position #\Newline text) (length text)))))
    (subseq line 0 (min 200 (length line)))))

(defun %search-result-text (query matches missing pending-count)
  (with-output-to-string (s)
    (if matches
        (progn
          (format s "Loaded ~d tool~:p; they can be called from the next step:" (length matches))
          (dolist (tool matches)
            (format s "~%- ~a: ~a" (llm-tool-name tool) (%first-line (llm-tool-description tool)))))
        (format s "No deferred tool matches \"~a\" (~d deferred). Try other words or select:NAME."
                query pending-count))
    (when missing
      (format s "~%Not found: ~{~a~^, ~}" missing))))

(defun %invoke-search (source deferrable arguments callback error-callback)
  "The search call: load the matches, ON-LOAD, then CALLBACK with the result
text on the loop. Bad arguments: their AGENT-ERROR to ERROR-CALLBACK. → NIL."
  (let ((ok (or callback (lambda (value) (declare (ignore value)))))
        (err (or error-callback #'error)))
    (multiple-value-bind (query max condition)
        (handler-case (%search-arguments source arguments)
          (agent-error (c) (values nil nil c)))
      (if condition
          (%call-on-loop (lambda () (funcall err condition)))
          (multiple-value-bind (matches missing)
              (tool-search-matches deferrable query :max-results max)
            (let* ((pending-count (length (%deferred-pending-names source deferrable)))
                   (ordered (loop for tool in deferrable
                                  when (member tool matches :test #'eq)
                                    collect (llm-tool-name tool)))
                   (new (%deferred-add-loaded source ordered))
                   (text (%search-result-text query matches missing pending-count)))
              (%deferred-notify source new :search)
              (%call-on-loop (lambda () (funcall ok text))))))))
  nil)

(defmethod list-agent-tools ((source deferred-tool-source) &key context)
  (multiple-value-bind (tools deferrable active) (%deferral source context)
    (if (not active)
        tools
        (let ((pending (%deferred-pending-names source deferrable)))
          (append (remove-if (lambda (tool) (member (llm-tool-name tool) pending :test #'equal))
                             tools)
                  (list (%search-tool source pending)))))))

(defmethod tool-executable-p ((source deferred-tool-source) name &key context)
  (if (and (%search-name-p source name)
           (deferred-tool-source-active-p source :context context))
      t
      ;; INNER's answer: a deferred tool stays executable (auto-load), a name
      ;; INNER lacks does not.
      (call-next-method)))

(defmethod tool-needs-approval-p ((source deferred-tool-source) name arguments &key context)
  (if (and (%search-name-p source name)
           (deferred-tool-source-active-p source :context context))
      nil
      ;; INNER's approval; asking loads nothing.
      (call-next-method)))

(defmethod invoke-tool-async ((source deferred-tool-source) name arguments
                              &key context callback error-callback)
  (multiple-value-bind (tools deferrable active) (%deferral source context)
    (declare (ignore tools))
    (cond
      ((and active (%search-name-p source name))
       (%invoke-search source deferrable arguments callback error-callback))
      (t
       ;; Auto-load: only a call the permission path let run reaches here.
       (when (and active (%find-tool name deferrable))
         (%deferred-notify source (%deferred-add-loaded source (list name)) :call))
       (call-next-method)))))
