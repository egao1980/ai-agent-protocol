(in-package #:ai-agent-protocol/mcp)

;;; MCP sampling lives here — not in llm-protocol.

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

(defun make-mcp-sampling-handler (&key (backend llm-protocol:*llm-backend*))
  "Host create-message → GENERATE. Not an llm-protocol concern."
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

;;; MCP peer as a tool source (list-tools / call-tool).

(defclass mcp-tool-source ()
  ((peer :initarg :peer :accessor mcp-tool-source-peer)))

(defun make-mcp-tool-source (peer)
  (make-instance 'mcp-tool-source :peer peer))

(defun %mcp-result-text (result)
  (cond
    ((stringp result) result)
    ((hash-table-p result)
     (let ((content (gethash "content" result)))
       (cond
         ((and (or (vectorp content) (listp content))
               (plusp (length content)))
          (or (gethash "text" (elt (coerce content 'vector) 0))
              (princ-to-string result)))
         (t (princ-to-string result)))))
    (t (princ-to-string result))))

(defmethod list-agent-tools ((source mcp-tool-source) &key context)
  (declare (ignore context))
  (mapcar (lambda (tool)
            (make-llm-tool :name (mcp-protocol:mcp-tool-name tool)
                           :description (mcp-protocol:mcp-tool-description tool)
                           :parameters (mcp-protocol:mcp-tool-input-schema tool)))
          (mcp-protocol:list-tools (mcp-tool-source-peer source))))

(defmethod tool-executable-p ((source mcp-tool-source) name &key context)
  (declare (ignore context))
  (and (find name (list-agent-tools source) :key #'llm-tool-name :test #'equal) t))

(defmethod invoke-tool-async ((source mcp-tool-source) name arguments
                              &key context callback error-callback)
  (declare (ignore context))
  (let ((ok (or callback (lambda (v) (declare (ignore v)))))
        (err (or error-callback #'error)))
    (ai-agent-protocol::%off-loop
     (lambda ()
       (%mcp-result-text
        (mcp-protocol:call-tool (mcp-tool-source-peer source) name arguments)))
     ok err)))
