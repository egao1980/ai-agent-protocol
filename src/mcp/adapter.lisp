(in-package #:ai-agent-protocol/mcp)

;;; MCP sampling lives here — not in llm-protocol.
;;;
;;; 0.3.0 (contract, ai-agent-protocol 0.17.0; cl-stack-code H3e-2b): the host
;;; decides the model and the budget, never the server.
;;;   MAKE-MCP-SAMPLING-HANDLER &key (BACKEND *LLM-BACKEND*) MODEL MAX-TOKENS
;;;   → (lambda (params)) for MCP-CLIENT-SAMPLING-HANDLER:
;;;     MODEL   the :MODEL of GENERATE. The request's modelPreferences (hints,
;;;             priorities) are never a model name: 0.2 passed the first hint as
;;;             the model, so a server chose among the host's models. NIL →
;;;             NIL (the backend's default model).
;;;     MAX-TOKENS  the cap: :MAX-TOKENS of the settings is min(the request's
;;;             maxTokens, MAX-TOKENS), MAX-TOKENS when the request has none, the
;;;             request's when MAX-TOKENS is NIL.
;;;     Content: a message's text content as 0.2; an image content item
;;;             {"type": "image", "data", "mimeType"} → an LLM-IMAGE-PART (data
;;;             and media type as given) in that turn; audio → the text part
;;;             "[audio omitted]". includeContext is ignored (the host never
;;;             shares its conversation through it).
;;;     The result's "model": the response's model, else MODEL, else omitted.
;;;   GIVEN: the keys (ignored until implemented: today's behaviour). STUBBED:
;;;   nothing else; the changes are the implementation's.

(defun %hint-name (prefs)
  (cond
    ((null prefs) nil)
    ((stringp prefs) prefs)
    ((typep prefs 'mcp-protocol:mcp-model-preferences)
     (%hint-name (mcp-protocol::mcp-model-preferences-hints prefs)))
    ((hash-table-p prefs)
     (%hint-name (or (gethash "hints" prefs) (gethash "name" prefs))))
    ((or (vectorp prefs) (listp prefs))
     (let ((first (car (llm-protocol::%as-list prefs))))
       (cond
         ((null first) nil)
         ((stringp first) first)
         ((hash-table-p first) (or (gethash "name" first) (gethash "model" first)))
         ((typep first 'mcp-protocol:mcp-model-preferences)
          (%hint-name first))
         (t nil))))
    (t nil)))

(defun %sampling-model (params)
  (cond
    ((typep params 'mcp-protocol:mcp-sampling-request)
     (%hint-name (mcp-protocol::mcp-sampling-request-model-preferences params)))
    (t
     (or (mcp-protocol:param params "model")
         (%hint-name (mcp-protocol:param params "modelPreferences"))))))

(defun %sampling-slot (params json-key accessor)
  (if (typep params 'mcp-protocol:mcp-sampling-request)
      (funcall accessor params)
      (mcp-protocol:param params json-key)))

(defun %mcp-message->turn (msg)
  (cond
    ((typep msg 'mcp-protocol:mcp-sampling-message)
     (make-llm-turn
      :role (llm-protocol::%role
             (or (mcp-protocol::mcp-sampling-message-role msg) :user))
      :parts (list (make-llm-text-part
                    :text (llm-protocol::%content-text
                           (mcp-protocol::mcp-sampling-message-content msg))))))
    (t (coerce-turn msg))))

(defun %sampling-turns (params)
  (let* ((raw (if (typep params 'mcp-protocol:mcp-sampling-request)
                  (mcp-protocol::mcp-sampling-request-messages params)
                  (mcp-protocol:param params "messages")))
         (turns (mapcar #'%mcp-message->turn (llm-protocol::%as-list raw)))
         (sys (if (typep params 'mcp-protocol:mcp-sampling-request)
                  (mcp-protocol::mcp-sampling-request-system-prompt params)
                  (or (mcp-protocol:param params "systemPrompt")
                      (mcp-protocol:param params "system")))))
    (if (and sys (plusp (length (string sys))))
        (cons (system-turn sys) turns)
        turns)))

(defun %sampling-settings (params)
  (make-llm-settings
   :temperature (%sampling-slot params "temperature"
                                #'mcp-protocol::mcp-sampling-request-temperature)
   :max-tokens (%sampling-slot params "maxTokens"
                               #'mcp-protocol::mcp-sampling-request-max-tokens)
   :stop (%sampling-slot params "stopSequences"
                         #'mcp-protocol::mcp-sampling-request-stop-sequences)))

(defun %stop-reason (finish)
  (case finish
    ((:stop nil) "endTurn")
    (:length "maxTokens")
    (:tool-use "endTurn")
    (t (if (stringp finish) finish "endTurn"))))

(defun llm-response->mcp-create-message (response)
  "Map GENERATE's LLM-RESPONSE to a sampling/createMessage result object."
  (mcp-protocol:json-object
   "role" "assistant"
   "model" (or (llm-response-model response) :omit)
   "content" (mcp-protocol:make-text-content (or (llm-response-text response) ""))
   "stopReason" (%stop-reason (llm-response-finish-reason response))))

(defun make-mcp-sampling-handler (&key (backend llm-protocol:*llm-backend*) model max-tokens)
  "Host create-message → GENERATE. Not an llm-protocol concern. MODEL and
MAX-TOKENS: 0.3.0 (see the header)."
  (declare (ignorable model max-tokens))
  (lambda (params)
    (llm-response->mcp-create-message
     (generate (or backend llm-protocol:*llm-backend*)
               (%sampling-turns params)
               :model (%sampling-model params)
               :settings (%sampling-settings params)
               :tools (%sampling-slot params "tools"
                                      #'mcp-protocol::mcp-sampling-request-tools)
               :tool-choice (%sampling-slot params "toolChoice"
                                            #'mcp-protocol::mcp-sampling-request-tool-choice)))))

