(in-package #:ai-agent-protocol)

;;; 0.15.0 (contract, egao1980/ai-agent-protocol#37): tool-source combinators.
;;; Prior art: pydantic-ai's toolsets (CombinedToolset, PrefixedToolset,
;;; RenamedToolset, FilteredToolset, PreparedToolset, ApprovalRequiredToolset,
;;; WrapperToolset; pydantic.dev/docs/ai/tools-toolsets/toolsets). Here they are
;;; CLOS sources with methods on the four tool-source generic functions
;;; (LIST-AGENT-TOOLS, TOOL-EXECUTABLE-P, TOOL-NEEDS-APPROVAL-P,
;;; INVOKE-TOOL-ASYNC), so they nest, and nothing in them is MCP-specific.
;;; ai-agent-protocol/mcp's MCP-TOOL-SOURCE is one source; a host offers server
;;; "lisp" as (make-prefixed-tool-source (make-mcp-tool-source client) "mcp__lisp__").
;;;
;;; WRAPPER-TOOL-SOURCE (base class, slot INNER, reader TOOL-SOURCE-INNER)
;;;   Every one of the four GFs delegates to INNER unchanged (CONTEXT, CALLBACK
;;;   and ERROR-CALLBACK passed through). Subclass it for logging, metrics or
;;;   timeouts and override one method. Every combinator below is one.
;;;
;;; COMBINED-TOOL-SOURCE (MAKE-COMBINED-TOOL-SOURCE &rest SOURCES)
;;;   The tools of SOURCES in order. A name offered by two sources signals
;;;   TOOL-NAME-CONFLICT (an AGENT-ERROR: the name and both sources) from
;;;   LIST-AGENT-TOOLS, with restart CONTINUE (keep the first source's tool,
;;;   drop the later one). The other three GFs go to the source that lists the
;;;   name (the first one when CONTINUE dropped a duplicate); TOOL-EXECUTABLE-P
;;;   is NIL for a name no source lists, and INVOKE-TOOL-ASYNC reports
;;;   AGENT-UNKNOWN-TOOL to ERROR-CALLBACK for it (as the default method).
;;;
;;; PREFIXED-TOOL-SOURCE (MAKE-PREFIXED-TOOL-SOURCE INNER PREFIX)
;;;   Each tool's name is PREFIX + its name (a fresh LLM-TOOL; description and
;;;   parameters shared). The other GFs strip PREFIX before delegating; a name
;;;   without it is not executable (unknown tool).
;;;
;;; RENAMED-TOOL-SOURCE (MAKE-RENAMED-TOOL-SOURCE INNER ALIST)
;;;   ALIST of (NEW-NAME . OLD-NAME). A listed tool whose name is an OLD-NAME is
;;;   offered as NEW-NAME; other tools keep their names. Calls map NEW → OLD; an
;;;   OLD-NAME that was renamed is no longer callable under its old name.
;;;
;;; FILTERED-TOOL-SOURCE (MAKE-FILTERED-TOOL-SOURCE INNER PREDICATE)
;;;   Offers the tools for which (TOOL-PREDICATE-P PREDICATE CONTEXT TOOL) is
;;;   true, evaluated on every LIST-AGENT-TOOLS call (every step: CONTEXT is
;;;   what the loop passes, possibly NIL). A filtered-out name is not
;;;   executable.
;;;
;;; PREPARED-TOOL-SOURCE (MAKE-PREPARED-TOOL-SOURCE INNER FUNCTION)
;;;   (FUNCALL FUNCTION CONTEXT TOOLS) → the tool definitions to offer, every
;;;   step: it may change descriptions and parameters and drop tools. A result
;;;   that adds or renames a tool (a name INNER did not list) signals
;;;   AGENT-ERROR "prepared tools may not add or rename: <names>".
;;;
;;; APPROVAL-REQUIRED-TOOL-SOURCE (MAKE-APPROVAL-REQUIRED-TOOL-SOURCE INNER
;;;   PREDICATE)
;;;   TOOL-NEEDS-APPROVAL-P is true when INNER's is, or when
;;;   (TOOL-PREDICATE-P PREDICATE CONTEXT TOOL ARGUMENTS) is (TOOL: the
;;;   LLM-TOOL INNER lists under that name; ARGUMENTS as the loop passes them).
;;;
;;; Predicates (TOOL-PREDICATE-P PREDICATE CONTEXT TOOL &optional ARGUMENTS)
;;;   A generic function. A FUNCTION is called with (CONTEXT TOOL) for a
;;;   filter and (CONTEXT TOOL ARGUMENTS) for an approval (the source passes
;;;   ARGUMENTS only for approvals), its value as a generalized boolean.
;;;   Rules that encode permissions must be policies (cl-stack's
;;;   rules-and-logic rule): the ai-agent-protocol/policy system adds methods
;;;   for POLICY-PROTOCOL:POLICY and LAYERED-POLICY (src/policy.lisp).
;;;
;;; Rich tool output (AGENT-TOOL-OUTPUT)
;;;   INVOKE-TOOL-ASYNC's CALLBACK may receive an AGENT-TOOL-OUTPUT (given:
;;;   MAKE-AGENT-TOOL-OUTPUT &key TEXT PARTS STRUCTURED) instead of a string.
;;;   The loop records TEXT as the invocation's result (what the model reads
;;;   in the tool result, as before), PARTS (LLM-IMAGE-PARTs and other
;;;   non-text LLM-PARTs) as AGENT-INVOCATION-PARTS and STRUCTURED (a JSON
;;;   object) as AGENT-INVOCATION-STRUCTURED. Tool results stay strings on the
;;;   wire (llm-protocol's LLM-TOOL-RESULT-PART holds text), so when a step's
;;;   invocations carry parts, the next GENERATE gets one :user turn right
;;;   after that step's tool results: for each such invocation in order a text
;;;   part "[output of tool <name> (<id>)]" followed by its parts. Durable
;;;   journals record TEXT only (a replay has no parts).

(define-condition tool-name-conflict (agent-error)
  ((name :initarg :name :reader tool-name-conflict-name)
   (sources :initarg :sources :reader tool-name-conflict-sources))
  (:report (lambda (c s)
             (format s "tool ~s is offered by two sources" (tool-name-conflict-name c)))))

(defun %toolsets-pending (what)
  "Contract stub (0.15.0). The implementation removes it and every call."
  (error 'agent-error :message (format nil "not implemented yet (0.15.0 contract): ~a" what)))

(defclass wrapper-tool-source ()
  ((inner :initarg :inner :reader tool-source-inner))
  (:documentation "Delegates every tool-source GF to INNER."))

(defclass combined-tool-source ()
  ((sources :initarg :sources :reader combined-tool-source-sources)))

(defclass prefixed-tool-source (wrapper-tool-source)
  ((prefix :initarg :prefix :reader prefixed-tool-source-prefix)))

(defclass renamed-tool-source (wrapper-tool-source)
  ((names :initarg :names :reader renamed-tool-source-names
          :documentation "Alist (NEW-NAME . OLD-NAME).")))

(defclass filtered-tool-source (wrapper-tool-source)
  ((predicate :initarg :predicate :reader filtered-tool-source-predicate)))

(defclass prepared-tool-source (wrapper-tool-source)
  ((function :initarg :function :reader prepared-tool-source-function)))

(defclass approval-required-tool-source (wrapper-tool-source)
  ((predicate :initarg :predicate :reader approval-required-tool-source-predicate)))

(defun make-wrapper-tool-source (inner)
  (make-instance 'wrapper-tool-source :inner inner))
(defun make-combined-tool-source (&rest sources)
  (make-instance 'combined-tool-source :sources sources))
(defun make-prefixed-tool-source (inner prefix)
  (check-type prefix string)
  (make-instance 'prefixed-tool-source :inner inner :prefix prefix))
(defun make-renamed-tool-source (inner alist)
  (make-instance 'renamed-tool-source :inner inner :names alist))
(defun make-filtered-tool-source (inner predicate)
  (make-instance 'filtered-tool-source :inner inner :predicate predicate))
(defun make-prepared-tool-source (inner function)
  (make-instance 'prepared-tool-source :inner inner :function function))
(defun make-approval-required-tool-source (inner predicate)
  (make-instance 'approval-required-tool-source :inner inner :predicate predicate))

(defgeneric tool-predicate-p (predicate context tool &optional arguments)
  (:documentation "True when PREDICATE holds for TOOL (an LLM-TOOL) in CONTEXT;
ARGUMENTS for approval predicates. Methods: FUNCTION here, policies in
ai-agent-protocol/policy."))

(defmethod tool-predicate-p ((predicate function) context tool &optional (arguments nil arguments-p))
  (declare (ignore context tool arguments arguments-p))
  (%toolsets-pending "tool-predicate-p"))

;;; Rich output (given)

(defclass agent-tool-output ()
  ((text :initarg :text :reader agent-tool-output-text :initform "")
   (parts :initarg :parts :reader agent-tool-output-parts :initform nil)
   (structured :initarg :structured :reader agent-tool-output-structured :initform nil))
  (:documentation "A tool's result with non-text parts and structured content (0.15.0)."))

(defun make-agent-tool-output (&key (text "") parts structured)
  (make-instance 'agent-tool-output :text text :parts parts :structured structured))

;;; Stubbed methods (the behaviour is the header's)

(macrolet ((stub-methods (class)
             `(progn
                (defmethod list-agent-tools ((source ,class) &key context)
                  (declare (ignore context))
                  (%toolsets-pending (format nil "list-agent-tools ~(~a~)" ',class)))
                (defmethod tool-executable-p ((source ,class) name &key context)
                  (declare (ignore name context))
                  (%toolsets-pending (format nil "tool-executable-p ~(~a~)" ',class)))
                (defmethod tool-needs-approval-p ((source ,class) name arguments &key context)
                  (declare (ignore name arguments context))
                  (%toolsets-pending (format nil "tool-needs-approval-p ~(~a~)" ',class)))
                (defmethod invoke-tool-async ((source ,class) name arguments
                                              &key context callback error-callback)
                  (declare (ignore name arguments context callback error-callback))
                  (%toolsets-pending (format nil "invoke-tool-async ~(~a~)" ',class))))))
  (stub-methods wrapper-tool-source)
  (stub-methods combined-tool-source))
