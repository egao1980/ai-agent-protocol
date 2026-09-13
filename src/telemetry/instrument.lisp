(in-package #:ai-agent-protocol/telemetry)

;;; Thin consumer instrumentation. New files only — do not touch src/loop.lisp.
;;; RUN-AI-AGENT is sync (%await on the loop), so WITH-SPAN covers the run.
;;; The loop calls INVOKE-TOOL-ASYNC; start/end the tool span in its callbacks.

(defmethod run-ai-agent :around ((agent ai-agent) turns &rest args)
  (declare (ignore args))
  (with-span ("invoke_agent")
    (instrument-gen-ai-span
     *current-span*
     :operation-name "invoke_agent"
     :agent-name (ai-agent-name agent)
     :request-model (let ((b (ai-agent-backend agent)))
                      (and b (backend-model b))))
    (call-next-method)))

(defmethod invoke-tool-async :around (source name arguments
                                      &key context callback error-callback)
  (let* ((be *telemetry-backend*)
         (span (start-span be "execute_tool")))
    (instrument-gen-ai-span
     span
     :operation-name "execute_tool"
     :tool-name (if (stringp name) name (princ-to-string name))
     :backend be)
    (flet ((finish (ok)
             (unless (telemetry-span-ended-p span)
               (end-span be span :status (if ok :ok :error)))))
      (call-next-method
       source name arguments
       :context context
       :callback (lambda (result)
                   (unwind-protect
                        (when callback (funcall callback result))
                     (finish t)))
       :error-callback (lambda (c)
                         (record-span-exception be span c)
                         (unwind-protect
                              (when error-callback (funcall error-callback c))
                           (finish nil)))))))
