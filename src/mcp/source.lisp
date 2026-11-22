(in-package #:ai-agent-protocol/mcp)

;;; 0.2.0 (contract, egao1980/ai-agent-protocol#37): an MCP peer as one tool
;;; source. Names are the server's own; a host namespaces them with
;;; AI-AGENT-PROTOCOL:MAKE-PREFIXED-TOOL-SOURCE (cl-stack-code: "mcp__<server>__").
;;; Needs mcp-protocol 0.4.0 (#14): every tool field, LIST-ALL-TOOLS, the
;;; tools generation, CALL-TOOL :TIMEOUT. Prior art: pydantic-ai's MCPServer
;;; toolset (cache_tools, tool_error_behavior, process_tool_call, timeouts).
;;;
;;; MAKE-MCP-TOOL-SOURCE PEER &key (CACHE-TOOLS T) (TOOL-ERROR-BEHAVIOR :RETRY)
;;;   PROCESS-TOOL-CALL TIMEOUT LIST-TIMEOUT
;;;   PEER: an MCP-CLIENT (or an MCP-SERVER, in process). An unknown
;;;   TOOL-ERROR-BEHAVIOR signals TYPE-ERROR.
;;;
;;; The tool list
;;;   MCP-TOOL-SOURCE-TOOLS SOURCE → the MCP-TOOLs, from
;;;   MCP-PROTOCOL:LIST-ALL-TOOLS (an MCP-SERVER peer: LIST-TOOLS), with
;;;   LIST-TIMEOUT. With CACHE-TOOLS the list is fetched once and kept until the
;;;   client's MCP-CLIENT-TOOLS-GENERATION changes (a list_changed
;;;   notification) or MCP-TOOL-SOURCE-INVALIDATE is called; without it every
;;;   call fetches (pydantic's caveat: modern servers send notifications only
;;;   while a request is open, so a host may turn caching off per server).
;;;   LIST-AGENT-TOOLS, TOOL-EXECUTABLE-P and FIND-MCP-TOOL all read this one
;;;   list: with the cache, TOOL-EXECUTABLE-P never makes a request.
;;;   LIST-AGENT-TOOLS → one LLM-TOOL per tool that
;;;   MCP-PROTOCOL:MCP-TOOL-VISIBLE-TO-MODEL-P accepts (app-only tools are
;;;   hidden and not executable), in server order: name, description (the
;;;   tool's description; its title when the description is empty), parameters
;;;   (the input schema, or {"type": "object"} when absent).
;;;   FIND-MCP-TOOL SOURCE NAME → the MCP-TOOL (annotations, title, output
;;;   schema, _meta), or NIL. MCP-TOOL-SOURCE-READ-ONLY-P SOURCE NAME →
;;;   MCP-PROTOCOL:MCP-TOOL-READ-ONLY-P of it (NIL when unknown).
;;;   MCP-TOOL-SOURCE-OUTPUT-SCHEMA SOURCE NAME → its output schema, or NIL
;;;   (llm-protocol's LLM-TOOL has no return-schema slot yet).
;;;   TOOL-NEEDS-APPROVAL-P: NIL (approval is the host's rules; wrap the source
;;;   in an APPROVAL-REQUIRED-TOOL-SOURCE for a predicate).
;;;
;;; A call (INVOKE-TOOL-ASYNC)
;;;   Off the loop. ARGUMENTS (a JSON string as the loop gives it, a hash
;;;   table, or NIL) are decoded to a JSON object ("" / NIL → {}; a string that
;;;   is not a JSON object → ERROR-CALLBACK with an AGENT-ERROR "arguments must
;;;   be a JSON object"). Then, with PROCESS-TOOL-CALL, (FUNCALL
;;;   PROCESS-TOOL-CALL CONTEXT CALL-FN NAME ARGUMENTS) where (FUNCALL CALL-FN
;;;   NAME ARGUMENTS) does the request; its value is the result object (so the
;;;   hook may add "_meta" to the arguments, rewrite them, or post-process the
;;;   result); without it, CALL-FN directly. The request is
;;;   MCP-PROTOCOL:CALL-TOOL with :TIMEOUT TIMEOUT.
;;;   A result → MCP-RESULT-OUTPUT result → an AGENT-TOOL-OUTPUT:
;;;     TEXT   every content item in order, joined by one LF: "text" items
;;;            as written; an embedded resource with "text" → that text; an
;;;            "image" → "[image <mimeType>]"; "audio" → "[audio <mimeType>]";
;;;            an embedded blob → "[resource <uri> <mimeType>]"; a
;;;            "resource_link" → "[resource <uri>]". No content but
;;;            "structuredContent" → its compact JSON. Nothing at all → "".
;;;     PARTS  one LLM-IMAGE-PART per "image" item (:media-type the mimeType,
;;;            :data the base64 string as given), in order.
;;;     STRUCTURED  "structuredContent" as given, or NIL.
;;;   "isError": true → TOOL-ERROR-BEHAVIOR decides:
;;;     :RETRY  (default) ERROR-CALLBACK with an MCP-TOOL-RESULT-ERROR whose
;;;             message is TEXT and RETRYABLE T: the loop records a tool error,
;;;             the model reads it and may try again; the run goes on.
;;;     :FAILED the same with RETRYABLE NIL: recorded as a failed call.
;;;     :ERROR  ERROR-CALLBACK with an MCP-TOOL-RUN-ERROR (an AGENT-ERROR that
;;;             is not an AGENT-TOOL-ERROR cause the loop may skip: the host's
;;;             handler sees it; unhandled it fails the run).
;;;   A transport or protocol failure (MCP-ERROR, MCP-TIMEOUT, RPC-ERROR) is
;;;   never a failed tool call of the server's: ERROR-CALLBACK gets that
;;;   condition itself.
;;;   Otherwise CALLBACK gets the AGENT-TOOL-OUTPUT when it has PARTS or
;;;   STRUCTURED, else the TEXT string.

(define-condition mcp-tool-result-error (agent-error)
  ((name :initarg :name :reader mcp-tool-result-error-name :initform nil)
   (result :initarg :result :reader mcp-tool-result-error-result :initform nil)
   (retryable :initarg :retryable :reader mcp-tool-result-error-retryable :initform t))
  (:report (lambda (c s) (format s "~a" (agent-error-message c))))
  (:documentation "An MCP tool answered isError: true (:RETRY / :FAILED)."))

(define-condition mcp-tool-run-error (agent-error)
  ((name :initarg :name :reader mcp-tool-run-error-name :initform nil)
   (result :initarg :result :reader mcp-tool-run-error-result :initform nil))
  (:report (lambda (c s) (format s "MCP tool ~a failed: ~a" (mcp-tool-run-error-name c)
                                 (agent-error-message c))))
  (:documentation "An MCP tool answered isError: true under :ERROR."))

(defparameter +tool-error-behaviors+ '(:retry :failed :error))

(defclass mcp-tool-source ()
  ((peer :initarg :peer :accessor mcp-tool-source-peer)
   (cache-tools :initarg :cache-tools :reader mcp-tool-source-cache-tools :initform t)
   (tool-error-behavior :initarg :tool-error-behavior
                        :reader mcp-tool-source-tool-error-behavior :initform :retry)
   (process-tool-call :initarg :process-tool-call
                      :reader mcp-tool-source-process-tool-call :initform nil)
   (timeout :initarg :timeout :reader mcp-tool-source-timeout :initform nil)
   (list-timeout :initarg :list-timeout :reader mcp-tool-source-list-timeout :initform nil)
   ;; the cache (internal)
   (cached-tools :initform nil :accessor %cached-tools)
   (cached-generation :initform nil :accessor %cached-generation)
   (lock :initform (bt2:make-lock :name "mcp-tool-source") :reader %source-lock)))

(defun make-mcp-tool-source (peer &key (cache-tools t) (tool-error-behavior :retry)
                                    process-tool-call timeout list-timeout)
  (unless (member tool-error-behavior +tool-error-behaviors+)
    (error 'type-error :datum tool-error-behavior
                       :expected-type `(member ,@+tool-error-behaviors+)))
  (make-instance 'mcp-tool-source :peer peer :cache-tools cache-tools
                                  :tool-error-behavior tool-error-behavior
                                  :process-tool-call process-tool-call
                                  :timeout timeout :list-timeout list-timeout))

(defun %source-pending (what)
  "Contract stub (0.2.0). The implementation removes it and every call."
  (error 'agent-error :message (format nil "not implemented yet (ai-agent-protocol/mcp 0.2.0 contract): ~a" what)))

(defun mcp-tool-source-tools (source)
  (declare (ignore source))
  (%source-pending "mcp-tool-source-tools"))

(defun mcp-tool-source-invalidate (source)
  (declare (ignore source))
  (%source-pending "mcp-tool-source-invalidate"))

(defun find-mcp-tool (source name)
  (declare (ignore source name))
  (%source-pending "find-mcp-tool"))

(defun mcp-tool-source-read-only-p (source name)
  (declare (ignore source name))
  (%source-pending "mcp-tool-source-read-only-p"))

(defun mcp-tool-source-output-schema (source name)
  (declare (ignore source name))
  (%source-pending "mcp-tool-source-output-schema"))

(defun mcp-result-output (result)
  (declare (ignore result))
  (%source-pending "mcp-result-output"))

(defmethod list-agent-tools ((source mcp-tool-source) &key context)
  (declare (ignore context))
  (%source-pending "list-agent-tools"))

(defmethod tool-executable-p ((source mcp-tool-source) name &key context)
  (declare (ignore name context))
  (%source-pending "tool-executable-p"))

(defmethod invoke-tool-async ((source mcp-tool-source) name arguments
                              &key context callback error-callback)
  (declare (ignore name arguments context callback error-callback))
  (%source-pending "invoke-tool-async"))
