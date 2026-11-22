(in-package #:ai-agent-protocol/policy)

;;; 0.15.0 (contract, #37): policies as tool predicates (cl-stack's
;;; rules-and-logic rule: rules that decide which tools a model sees or which
;;; calls need approval are policies, not hand-written COND chains).
;;;
;;; AGENT:TOOL-PREDICATE-P on a POLICY-PROTOCOL:POLICY or LAYERED-POLICY
;;;   evaluates (POLICY:EVALUATE-POLICY policy request) with request
;;;     :action    the tool's name (LLM-TOOL-NAME)
;;;     :resources ("*")
;;;     :context   (FUNCALL *TOOL-POLICY-CONTEXT* CONTEXT TOOL ARGUMENTS) when
;;;                that is a function, else NIL: a list of ground facts.
;;;   As a filter (no ARGUMENTS): true unless the effect is :FORBID (:PERMIT,
;;;   :ASK and :NO-DECISION offer the tool). As an approval (ARGUMENTS given):
;;;   true when the effect is :ASK or :FORBID (a forbidden call still needs a
;;;   human; denying is the host's permission rules' job).

(defvar *tool-policy-context* nil
  "NIL, or (lambda (context tool arguments)) → context facts for the policy request.")

(defun %policy-pending (what)
  (error 'agent:agent-error :message (format nil "not implemented yet (0.15.0 contract): ~a" what)))

(defmethod agent:tool-predicate-p ((predicate policy:policy) context tool &optional (arguments nil arguments-p))
  (declare (ignore context tool arguments arguments-p))
  (%policy-pending "tool-predicate-p policy"))

(defmethod agent:tool-predicate-p ((predicate policy:layered-policy) context tool &optional (arguments nil arguments-p))
  (declare (ignore context tool arguments arguments-p))
  (%policy-pending "tool-predicate-p layered-policy"))
