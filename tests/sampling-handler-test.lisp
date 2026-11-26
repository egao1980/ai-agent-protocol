(in-package #:ai-agent-protocol/tests)

;;; 0.17.0 / ai-agent-protocol/mcp 0.3.0 (H3e-2b): the sampling handler's
;;; :MODEL and :MAX-TOKENS, image content (src/mcp/adapter.lisp).

(defun %recording-backend (cell)
  "A mock backend whose GENERATE records (turns model settings) in CELL's car."
  (make-mock-llm-backend
   :handler (lambda (backend turns &key model settings tools tool-choice)
              (declare (ignore backend tools tool-choice))
              (setf (car cell) (list turns model settings))
              (make-llm-response :parts (list (make-llm-text-part :text "sampled"))
                                 :model (or model "mock") :finish-reason :stop))))

(defun %sampling-params (&key (max-tokens 10000) (hint "gpt-x") content)
  (mcp-protocol:json-object
   "messages" (vector (mcp-protocol:json-object
                       "role" "user"
                       "content" (or content (mcp-protocol:make-text-content "ping"))))
   "maxTokens" max-tokens
   "modelPreferences" (mcp-protocol:json-object
                       "hints" (vector (mcp-protocol:json-object "name" hint)))))

(deftest sampling-handler-model-is-the-hosts
  (let* ((cell (list nil))
         (out (funcall (ai-agent-protocol/mcp:make-mcp-sampling-handler
                        :backend (%recording-backend cell) :model "small")
                       (%sampling-params))))
    (ok (equal "small" (second (car cell))) "the host's model, not the hint gpt-x")
    (ok (equal "small" (gethash "model" out))))
  (let ((cell (list nil)))
    (funcall (ai-agent-protocol/mcp:make-mcp-sampling-handler :backend (%recording-backend cell))
             (%sampling-params))
    (ok (null (second (car cell))) "no :MODEL: the backend's default, never the hint")))

(deftest-parametrize sampling-handler-max-tokens
    ((cap requested expected)
     (256 10000 256)
     (256 100 100)
     (nil 10000 10000)
     (256 nil 256))
  (let ((cell (list nil)))
    (funcall (ai-agent-protocol/mcp:make-mcp-sampling-handler :backend (%recording-backend cell)
                                                               :max-tokens cap)
             (let ((p (%sampling-params :max-tokens (or requested 1))))
               (unless requested (remhash "maxTokens" p))
               p))
    (ok (eql expected (llm-settings-max-tokens (third (car cell)))))))

(deftest sampling-handler-image-content
  (let ((cell (list nil)))
    (funcall (ai-agent-protocol/mcp:make-mcp-sampling-handler :backend (%recording-backend cell))
             (%sampling-params :content (mcp-protocol:json-object "type" "image" "data" "iVBORw0KGgo="
                                                                  "mimeType" "image/png")))
    (let* ((turn (find :user (first (car cell)) :key #'llm-turn-role))
           (image (find-if #'llm-image-part-p (llm-turn-parts turn))))
      (ok image "an image part")
      (ok (and image (equal "image/png" (llm-image-part-media-type image))))
      (ok (and image (equal "iVBORw0KGgo=" (llm-image-part-data image)))))))
