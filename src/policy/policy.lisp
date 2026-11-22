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

(defun %tool-policy-effect (policy context tool arguments)
  "The DECISION-EFFECT of POLICY for a call of TOOL (an LLM-TOOL) in CONTEXT."
  (let ((facts-fn *tool-policy-context*))
    (policy:decision-effect
     (policy:evaluate-policy
      policy
      (policy:make-policy-request
       :action (llm-protocol:llm-tool-name tool)
       :resources '("*")
       :context (and (functionp facts-fn)
                     (funcall facts-fn context tool arguments)))))))

(defun %policy-holds-p (policy context tool arguments arguments-p)
  "The header's projection of a decision: as an approval (ARGUMENTS-P), true
for :ASK and :FORBID; as a filter, true unless :FORBID."
  (let ((effect (%tool-policy-effect policy context tool arguments)))
    (if arguments-p
        (and (member effect '(:ask :forbid)) t)
        (not (eq effect :forbid)))))

(defmethod agent:tool-predicate-p ((predicate policy:policy) context tool &optional (arguments nil arguments-p))
  (%policy-holds-p predicate context tool arguments arguments-p))

(defmethod agent:tool-predicate-p ((predicate policy:layered-policy) context tool &optional (arguments nil arguments-p))
  (%policy-holds-p predicate context tool arguments arguments-p))
