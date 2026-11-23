(in-package #:ai-agent-protocol)

;;; 0.17.0 (contract, egao1980/ai-agent-protocol H3e-2b issue; after 0.16.0,
;;; #39 / #40): deferred tool loading. Prior art: pydantic-ai's deferred
;;; loading behind a search tool, Claude Code's ToolSearch ("select:A,B" and
;;; keyword queries). Found while contracting cl-stack-code's MCP part 2
;;; (H3e-2b, brief: cl-stack docs/capabilities/cl-stack-code.md "H3e-2b —
;;; locked"; this library's brief: docs/capabilities/ai-agent.md "0.17.0").
;;; GIVEN: the class, its constructor and readers, the condition, the
;;; constants. STUBBED: every method and function below. Nothing in it is
;;; MCP-specific: any source can be deferred.
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
;;;   order) is called when there are any, before the callback. The result
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
;;;   A deferrable tool not loaded (while active): TOOL-EXECUTABLE-P NIL;
;;;   INVOKE-TOOL-ASYNC → ERROR-CALLBACK gets AGENT-DEFERRED-TOOL-ERROR (an
;;;   AGENT-UNKNOWN-TOOL-ERROR, so the loop treats it as any unknown tool),
;;;   message "tool NAME is deferred: call SEARCH-TOOL-NAME with select:NAME
;;;   first". Every other name delegates to INNER (as a wrapper).
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
;;; Non-goals: embeddings or BM25 ranking (a host may wrap the source and rank
;;; its own way), deferring per run (the set is the source's), unloading.

(defparameter +tool-search-name+ "tool_search")

(defparameter +tool-search-default-results+ 5)

(defparameter +tool-search-description+
  "Search the deferred tools and load the matches: they can be called from the next step. Query with words from a tool's name or description, or select:NAME,NAME for exact names.")

(define-condition agent-deferred-tool-error (agent-unknown-tool-error) ()
  (:documentation "0.17.0: a call of a deferred tool that is not loaded."))

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

(defun %deferred-pending (what)
  (error 'agent-error :message (format nil "0.17.0 contract: ~a is not implemented" what)))

(defun deferred-tool-source-loaded (source)
  "→ the loaded names, in load order."
  (declare (ignore source))
  (%deferred-pending "deferred-tool-source-loaded"))

(defun (setf deferred-tool-source-loaded) (names source)
  (declare (ignore names source))
  (%deferred-pending "(setf deferred-tool-source-loaded)"))

(defun deferred-tool-source-active-p (source &key context)
  (declare (ignore source context))
  (%deferred-pending "deferred-tool-source-active-p"))

(defun deferred-tool-source-deferred (source &key context)
  (declare (ignore source context))
  (%deferred-pending "deferred-tool-source-deferred"))

(defun tool-search-matches (tools query &key (max-results +tool-search-default-results+))
  (declare (ignore tools query max-results))
  (%deferred-pending "tool-search-matches"))

(defmethod list-agent-tools ((source deferred-tool-source) &key context)
  (declare (ignore context))
  (%deferred-pending "list-agent-tools (deferred-tool-source)"))

(defmethod tool-executable-p ((source deferred-tool-source) name &key context)
  (declare (ignore name context))
  (%deferred-pending "tool-executable-p (deferred-tool-source)"))

(defmethod tool-needs-approval-p ((source deferred-tool-source) name arguments &key context)
  (declare (ignore name arguments context))
  (%deferred-pending "tool-needs-approval-p (deferred-tool-source)"))

(defmethod invoke-tool-async ((source deferred-tool-source) name arguments
                              &key context callback error-callback)
  (declare (ignore name arguments context callback error-callback))
  (%deferred-pending "invoke-tool-async (deferred-tool-source)"))
