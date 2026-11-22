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

;;; Peers: an MCP-CLIENT (requests, :TIMEOUT, a tools generation) or an
;;; in-process MCP-SERVER (no keys, no change signal: read every time).

(defgeneric %peer-tools (peer source)
  (:documentation "→ the MCP-TOOLs PEER offers, fetched now."))

(defmethod %peer-tools ((peer mcp-protocol:mcp-client) source)
  (mcp-protocol:list-all-tools peer :timeout (mcp-tool-source-list-timeout source)))

(defmethod %peer-tools ((peer mcp-protocol:mcp-server) source)
  (declare (ignore source))
  (mcp-protocol:list-tools peer))

(defgeneric %peer-call (peer name arguments source)
  (:documentation "→ the result object of one tools/call of NAME on PEER."))

(defmethod %peer-call ((peer mcp-protocol:mcp-client) name arguments source)
  (mcp-protocol:call-tool peer name arguments :timeout (mcp-tool-source-timeout source)))

(defmethod %peer-call ((peer mcp-protocol:mcp-server) name arguments source)
  ;; The server's CALL-TOOL takes no keys: no :TIMEOUT in process.
  (declare (ignore source))
  (mcp-protocol:call-tool peer name arguments))

(defgeneric %peer-generation (peer)
  (:documentation "PEER's tools generation, or NIL when it has no change signal
(the list is then read on every call).")
  (:method ((peer mcp-protocol:mcp-client))
    (mcp-protocol:mcp-client-tools-generation peer))
  (:method (peer)
    (declare (ignore peer))
    nil))

;;; The tool list

(defun mcp-tool-source-tools (source)
  "→ SOURCE's MCP-TOOLs: cached per tools generation with CACHE-TOOLS, else
fetched on every call."
  (let* ((peer (mcp-tool-source-peer source))
         (generation (%peer-generation peer)))
    (if (not (and generation (mcp-tool-source-cache-tools source)))
        (%peer-tools peer source)
        (bt2:with-lock-held ((%source-lock source))
          ;; GENERATION is read before the fetch: a list_changed that lands
          ;; during it makes the next call fetch again.
          (unless (eql generation (%cached-generation source))
            (setf (%cached-tools source) (%peer-tools peer source)
                  (%cached-generation source) generation))
          (%cached-tools source)))))

(defun mcp-tool-source-invalidate (source)
  "Drop SOURCE's cached tool list: the next read fetches it. → SOURCE."
  (bt2:with-lock-held ((%source-lock source))
    (setf (%cached-generation source) nil
          (%cached-tools source) nil))
  source)

(defun find-mcp-tool (source name)
  "→ the MCP-TOOL SOURCE's peer offers as NAME, or NIL."
  (find name (mcp-tool-source-tools source)
        :key #'mcp-protocol:mcp-tool-name :test #'equal))

(defun %model-tool (source name)
  "→ NAME's MCP-TOOL when the model may call it (not app-only), else NIL."
  (let ((tool (find-mcp-tool source name)))
    (and tool (mcp-protocol:mcp-tool-visible-to-model-p tool) tool)))

(defun mcp-tool-source-read-only-p (source name)
  (let ((tool (find-mcp-tool source name)))
    (and tool (mcp-protocol:mcp-tool-read-only-p tool) t)))

(defun mcp-tool-source-output-schema (source name)
  (let ((tool (find-mcp-tool source name)))
    (and tool (mcp-protocol:mcp-tool-output-schema tool))))

(defun %tool-description (tool)
  (let ((description (mcp-protocol:mcp-tool-description tool)))
    (if (and (stringp description) (plusp (length description)))
        description
        (or (mcp-protocol:mcp-tool-title tool) ""))))

(defmethod list-agent-tools ((source mcp-tool-source) &key context)
  (declare (ignore context))
  (loop for tool in (coerce (mcp-tool-source-tools source) 'list)
        when (mcp-protocol:mcp-tool-visible-to-model-p tool)
          collect (make-llm-tool
                   :name (mcp-protocol:mcp-tool-name tool)
                   :description (%tool-description tool)
                   :parameters (or (mcp-protocol:mcp-tool-input-schema tool)
                                   (mcp-protocol:json-object "type" "object")))))

(defmethod tool-executable-p ((source mcp-tool-source) name &key context)
  (declare (ignore context))
  (and (%model-tool source name) t))

;;; Results

(defun %json-null-p (x)
  (or (null x) (eq x :null) (eq x 'null)))

(defun %json-backend ()
  "json-protocol's ENCODE / DECODE need a bound *JSON-BACKEND* (soft use, as
llm-protocol). Signals TOOL-UNAVAILABLE when none is loaded."
  (let* ((pkg (find-package '#:json-protocol))
         (backend (and pkg (find-symbol "*JSON-BACKEND*" pkg))))
    (unless (and backend (boundp backend) (symbol-value backend))
      (error 'tool-unavailable :message "no json-protocol backend is loaded"))
    pkg))

(defun %json-text (value)
  "Compact JSON text of VALUE (json-protocol:encode)."
  (funcall (find-symbol "ENCODE" (%json-backend)) value))

(defun %content-text (item)
  "The text an MCP content ITEM contributes to a tool result, or NIL."
  (let ((type (mcp-protocol:param item "type")))
    (cond
      ((equal type "text") (or (mcp-protocol:param item "text") ""))
      ((equal type "image") (format nil "[image ~a]" (mcp-protocol:param item "mimeType")))
      ((equal type "audio") (format nil "[audio ~a]" (mcp-protocol:param item "mimeType")))
      ((equal type "resource")
       (let* ((resource (mcp-protocol:param item "resource"))
              (text (mcp-protocol:param resource "text")))
         (if (stringp text)
             text
             (format nil "[resource ~a~@[ ~a~]]"
                     (mcp-protocol:param resource "uri")
                     (mcp-protocol:param resource "mimeType")))))
      ((equal type "resource_link")
       (format nil "[resource ~a]" (mcp-protocol:param item "uri")))
      (t nil))))

(defun mcp-result-output (result)
  "→ the AGENT-TOOL-OUTPUT of an MCP tools/call RESULT (header: TEXT, PARTS,
STRUCTURED)."
  (let* ((raw (mcp-protocol:param result "content"))
         (content (if (%json-null-p raw) '() (coerce raw 'list)))
         (structured (let ((s (mcp-protocol:param result "structuredContent")))
                       (and (not (%json-null-p s)) s))))
    (make-agent-tool-output
     :text (cond (content (format nil "~{~a~^~%~}" (remove nil (mapcar #'%content-text content))))
                 (structured (%json-text structured))
                 (t ""))
     :parts (loop for item in content
                  when (equal "image" (mcp-protocol:param item "type"))
                    collect (make-llm-image-part
                             :media-type (mcp-protocol:param item "mimeType")
                             :data (mcp-protocol:param item "data")))
     :structured structured)))

;;; A call

(defun %mcp-arguments (arguments)
  "→ ARGUMENTS as a JSON object (hash table): \"\" / NIL → {}. Anything that
is not a JSON object signals AGENT-ERROR."
  (cond
    ((hash-table-p arguments) arguments)
    ((or (null arguments)
         (and (stringp arguments)
              (zerop (length (string-trim '(#\Space #\Tab #\Return #\Newline) arguments)))))
     (mcp-protocol:json-object))
    ((stringp arguments)
     (%json-backend)
     (let ((parsed (try-parse-json-output arguments)))
       (unless (hash-table-p parsed)
         (error 'agent-error :message "arguments must be a JSON object"))
       parsed))
    (t (error 'agent-error :message "arguments must be a JSON object"))))

(defun %is-error (source name result text)
  "Signal the condition TOOL-ERROR-BEHAVIOR gives an isError RESULT."
  (let ((behavior (mcp-tool-source-tool-error-behavior source)))
    (ecase behavior
      ((:retry :failed)
       (error 'mcp-tool-result-error :name name :result result :message text
                                     :retryable (eq behavior :retry)))
      (:error
       (error 'mcp-tool-run-error :name name :result result :message text)))))

(defun %mcp-call (source name arguments context)
  "Off the loop: decode, call (through PROCESS-TOOL-CALL), map → the CALLBACK
value. Failures are signalled; transport conditions are not caught."
  (let* ((args (%mcp-arguments arguments))
         (peer (mcp-tool-source-peer source))
         (call-fn (lambda (n a) (%peer-call peer n a source)))
         (hook (mcp-tool-source-process-tool-call source))
         (result (if hook
                     (funcall hook context call-fn name args)
                     (funcall call-fn name args)))
         (out (mcp-result-output result)))
    (when (eq t (mcp-protocol:param result "isError"))
      (%is-error source name result (agent-tool-output-text out)))
    (if (or (agent-tool-output-parts out) (agent-tool-output-structured out))
        out
        (agent-tool-output-text out))))

(defmethod invoke-tool-async ((source mcp-tool-source) name arguments
                              &key context callback error-callback)
  (let ((ok (or callback (lambda (v) (declare (ignore v)))))
        (err (or error-callback #'error)))
    (ai-agent-protocol::%off-loop
     (lambda ()
       ;; The list read may make a request (no cache), so it is off the loop
       ;; too; its failures reach ERR as themselves. AGENT-UNKNOWN-TOOL is not
       ;; an ERROR, so an app-only or unknown NAME comes back as the condition
       ;; object and is reported on the loop below.
       (if (%model-tool source name)
           (%mcp-call source name arguments context)
           (make-condition 'agent-unknown-tool-error
                           :name name
                           :message (format nil "unknown MCP tool ~s" name))))
     (lambda (value)
       (if (typep value 'agent-unknown-tool-error)
           (funcall err value)
           (funcall ok value)))
     err)))
