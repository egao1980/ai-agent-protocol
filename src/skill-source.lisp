(in-package #:ai-agent-protocol)

;;; Thin adapter: a steer-protocol skill as a tool source (list-agent-tools /
;;; invoke-tool-async). Mirrors make-mcp-tool-source. No extra agent core.

(defclass skill-tool-source ()
  ((skill :initarg :skill :accessor skill-tool-source-skill)))

(defun make-skill-tool-source (skill)
  (check-type skill steer:steer-directive)
  (make-instance 'skill-tool-source :skill skill))

(defmethod list-agent-tools ((source skill-tool-source) &key context)
  (declare (ignore context))
  (copy-list (steer:skill-tools (skill-tool-source-skill source))))

(defmethod tool-executable-p ((source skill-tool-source) name &key context)
  (declare (ignore context))
  (and (steer:skill-tool-fn (skill-tool-source-skill source) name) t))

(defmethod invoke-tool-async ((source skill-tool-source) name arguments
                              &key context callback error-callback)
  (declare (ignore context))
  (let ((ok (or callback (lambda (v) (declare (ignore v)))))
        (err (or error-callback #'error))
        (fn (steer:skill-tool-fn (skill-tool-source-skill source) name)))
    (cond
      ((null fn)
       (%call-on-loop
        (lambda ()
          (funcall err (make-condition 'agent-unknown-tool :name name
                                       :message (format nil "no skill tool ~s"
                                                        name)))))
       nil)
      (t
       (%off-loop
        (lambda ()
          (let ((out (funcall fn arguments)))
            (if (stringp out) out (princ-to-string out))))
        ok err)))))
