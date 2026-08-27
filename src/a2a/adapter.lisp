(in-package #:ai-agent-protocol/a2a)

;;; A2A handler is (lambda (agent message task &key) …) and mutates TASK.

(defun a2a-ai-agent-handler (ai-agent &key settings)
  "A2A-AGENT-HANDLER: run AI-AGENT on MESSAGE-TEXT, complete the task."
  (lambda (agent message task &key)
    (declare (ignore agent))
    (let ((ar (run-ai-agent ai-agent
                            (or (a2a-protocol:message-text message) "")
                            :settings settings)))
      (setf (a2a-protocol:a2a-task-state task) :completed
            (a2a-protocol:a2a-task-artifacts task)
            (list (a2a-protocol:make-a2a-artifact
                   :name (ai-agent-name ai-agent)
                   :parts (list (a2a-protocol:make-text-part
                                 (or (agent-run-text ar) ""))))))
      task)))
