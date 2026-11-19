(in-package #:ai-agent-protocol)

;;; Permission rules (opencode v2 shape). A rule is {action, resource, effect}:
;;; ACTION globs the tool name, RESOURCE globs one string the call touches
;;; (a path, a command; TOOL-PERMISSION-RESOURCES derives them). Layers are
;;; concatenated lowest first (agent, then run settings) and the LAST matching
;;; rule wins. With several resources a deny on any wins, then an ask. No rule
;;; matches → the tool's own default (TOOL-NEEDS-APPROVAL-P → :ask, else
;;; :allow). A policy layer on top may only tighten (allow → ask/deny,
;;; ask → deny). :ask is the existing approval pause; :deny is a
;;; TOOL-PERMISSION-DENIED tool result, not an abort.

(deftype permission-effect ()
  '(member :allow :ask :deny))

(defclass permission-rule ()
  ((action :initarg :action :reader permission-rule-action :initform "*")
   (resource :initarg :resource :reader permission-rule-resource :initform "*")
   (effect :initarg :effect :reader permission-rule-effect)
   (feedback :initarg :feedback :reader permission-rule-feedback :initform nil
             :documentation "Text the model sees when this rule denies a call
(what to do instead)."))
  (:documentation "One permission rule. ACTION and RESOURCE are whole-value
globs: * is any run of characters (including /), ? is one character."))

(defmethod print-object ((rule permission-rule) stream)
  (print-unreadable-object (rule stream :type t)
    (format stream "~s ~s ~s" (permission-rule-action rule)
            (permission-rule-resource rule) (permission-rule-effect rule))))

(defun make-permission-rule (action resource effect &key feedback)
  "Rule letting (:allow), pausing (:ask) or refusing (:deny) tool ACTION on
RESOURCE. FEEDBACK (deny only) tells the model what to do instead."
  (check-type action string)
  (check-type resource string)
  (check-type effect permission-effect)
  (check-type feedback (or null string))
  (make-instance 'permission-rule :action action :resource resource
                                  :effect effect :feedback feedback))

(defun permission-rule-p (x)
  (typep x 'permission-rule))

(defun coerce-permission-rule (x)
  "A PERMISSION-RULE, or a list (action resource effect &key feedback)."
  (cond
    ((permission-rule-p x) x)
    ((and (consp x) (stringp (first x)))
     (destructuring-bind (action resource effect &key feedback) x
       (make-permission-rule action resource effect :feedback feedback)))
    (t (error 'agent-error :message (format nil "not a permission rule: ~s" x)))))

(defun coerce-permission-rules (x)
  "List of PERMISSION-RULEs from NIL, one rule, one rule list, or a list of
either."
  (cond
    ((null x) nil)
    ((permission-rule-p x) (list x))
    ((and (consp x) (stringp (first x))) (list (coerce-permission-rule x)))
    ((listp x) (mapcar #'coerce-permission-rule x))
    (t (error 'agent-error :message (format nil "not permission rules: ~s" x)))))

;;; --- matching ---------------------------------------------------------------

(defun %glob-match-p (pattern string)
  "Whole-value glob: * any run (including /), ? one character."
  (declare (type string pattern string))
  (let ((plen (length pattern))
        (slen (length string))
        (p 0) (s 0) (star nil) (mark 0))
    (declare (type fixnum plen slen p s mark))
    (loop
      (cond
        ((< s slen)
         (cond
           ((and (< p plen) (char= (char pattern p) #\*))
            (setf star p mark s)
            (incf p))
           ((and (< p plen)
                 (or (char= (char pattern p) #\?)
                     (char= (char pattern p) (char string s))))
            (incf p) (incf s))
           (star
            (setf p (1+ star))
            (incf mark)
            (setf s mark))
           (t (return nil))))
        (t
         (loop while (and (< p plen) (char= (char pattern p) #\*))
               do (incf p))
         (return (= p plen)))))))

(defun permission-glob-match-p (pattern string)
  "T when glob PATTERN matches all of STRING. Backslashes count as slashes.
A pattern ending in \" *\" also matches the bare command (\"git status *\"
matches \"git status\")."
  (let ((pattern (substitute #\/ #\\ pattern))
        (string (substitute #\/ #\\ string)))
    (or (%glob-match-p pattern string)
        (let ((n (length pattern)))
          (and (> n 2)
               (string= " *" pattern :start2 (- n 2))
               (string= string pattern :end2 (- n 2)))))))

(defun permission-rule-matches-p (rule action resource)
  "T when RULE's action glob matches ACTION and its resource glob RESOURCE."
  (and (permission-glob-match-p (permission-rule-action rule) action)
       (permission-glob-match-p (permission-rule-resource rule) resource)))

(defun %effect-rank (effect)
  (ecase effect (:allow 0) (:ask 1) (:deny 2)))

(defun %stricter-effect (a b)
  "The stricter of effects A and B; NIL counts as no opinion."
  (cond ((null a) b)
        ((null b) a)
        ((>= (%effect-rank a) (%effect-rank b)) a)
        (t b)))

(defun permission-rules-effect (rules action resources)
  "→ (values EFFECT RULE). Per resource the last rule in RULES matching ACTION
and it wins; across RESOURCES a deny wins, then an ask, then allow. EFFECT is
NIL when no rule decides (some resource matched nothing and none asks or
denies). RULE is the rule that decided."
  (let ((worst nil) (worst-rule nil) (undecided nil))
    (dolist (resource (or resources (list "*")))
      (let ((rule (find-if (lambda (r) (permission-rule-matches-p r action resource))
                           rules :from-end t)))
        (if (null rule)
            (setf undecided t)
            (let ((effect (permission-rule-effect rule)))
              (when (or (null worst)
                        (> (%effect-rank effect) (%effect-rank worst)))
                (setf worst effect worst-rule rule))))))
    (if (and undecided (eq worst :allow))
        (values nil nil)
        (values worst worst-rule))))

;;; --- resources ----------------------------------------------------------------

(defgeneric tool-permission-resources (source name arguments &key context)
  (:documentation "→ list of resource strings (paths, commands, URLs) a call of
NAME on SOURCE with ARGUMENTS touches; the permission rules match each. Default
is (\"*\"). Specialize for a source, or give a FUNCTION-TOOL
:permission-resources.")
  (:method (source name arguments &key context)
    (declare (ignore source name arguments context))
    (list "*"))
  (:method ((tool function-tool) name arguments &key context)
    (declare (ignore name context))
    (let ((fn (function-tool-permission-resources tool)))
      (or (and fn
               (let ((out (funcall fn arguments)))
                 (mapcar (lambda (r) (if (stringp r) r (princ-to-string r)))
                         (if (listp out) out (list out)))))
          (list "*")))))

;;; --- policy -------------------------------------------------------------------

(defvar *permission-policy* nil
  "Global policy layer over every run: rules, a function or NIL. It can only
tighten the rules' effect (see PERMISSION-POLICY-EFFECT).")

(defgeneric permission-policy-effect (policy action resources effect)
  (:documentation "→ (values EFFECT* RULE) the effect POLICY wants for ACTION
on RESOURCES when the rules decided EFFECT, or NIL for no opinion. The caller
keeps the stricter of EFFECT and EFFECT*, so a policy never loosens.
POLICY is NIL, a rule list, or (lambda (action resources effect)).")
  (:method ((policy null) action resources effect)
    (declare (ignore action resources effect))
    nil)
  (:method ((policy permission-rule) action resources effect)
    (declare (ignore effect))
    (permission-rules-effect (list policy) action resources))
  (:method ((policy list) action resources effect)
    (declare (ignore effect))
    (permission-rules-effect (coerce-permission-rules policy) action resources))
  (:method ((policy function) action resources effect)
    (funcall policy action resources effect)))

;;; --- saved rules: the "always" reply -------------------------------------------

(defclass permission-rule-store ()
  ()
  (:documentation "Where \"always\" approvals are saved, so a product can keep
them as project rules across runs. Specialize SAVE-PERMISSION-RULE and
LIST-PERMISSION-RULES. Saved rules only answer asks: a deny from any rule
layer or a policy still wins."))

(defgeneric save-permission-rule (store rule &key agent)
  (:documentation "Save RULE (an :allow from an \"always\" approval in a run of
AGENT) in STORE. → RULE. NIL STORE saves nothing.")
  (:method ((store null) rule &key agent)
    (declare (ignore agent))
    rule))

(defgeneric list-permission-rules (store &key agent)
  (:documentation "→ the PERMISSION-RULEs saved in STORE that apply to AGENT,
oldest first. Called on every ask, so cache in the store if reading is slow.")
  (:method ((store null) &key agent)
    (declare (ignore agent))
    nil))

(defclass in-memory-permission-rule-store (permission-rule-store)
  ((rules :initarg :rules :initform nil)
   (lock :initform (bt:make-lock "permission-rule-store")
         :reader %permission-store-lock))
  (:documentation "PERMISSION-RULE-STORE in a list, shared by every agent."))

(defun make-in-memory-permission-rule-store (&key rules)
  "Store holding RULES (rule objects or lists) to start with."
  (make-instance 'in-memory-permission-rule-store
                 :rules (coerce-permission-rules rules)))

(defun %same-rule-p (a b)
  (and (equal (permission-rule-action a) (permission-rule-action b))
       (equal (permission-rule-resource a) (permission-rule-resource b))
       (eq (permission-rule-effect a) (permission-rule-effect b))))

(defmethod save-permission-rule ((store in-memory-permission-rule-store) rule
                                 &key agent)
  (declare (ignore agent))
  (let ((rule (coerce-permission-rule rule)))
    (bt:with-lock-held ((%permission-store-lock store))
      (with-slots (rules) store
        (unless (find rule rules :test #'%same-rule-p)
          (setf rules (append rules (list rule))))))
    rule))

(defmethod list-permission-rules ((store in-memory-permission-rule-store)
                                  &key agent)
  (declare (ignore agent))
  (bt:with-lock-held ((%permission-store-lock store))
    (copy-list (slot-value store 'rules))))

(defgeneric tool-permission-always-resources (source name arguments &key context)
  (:documentation "→ resource globs an \"always\" approval of this call allows
from now on (one :allow rule each). Default: TOOL-PERMISSION-RESOURCES, the
exact resources. Specialize to widen, e.g. a shell command to \"git status *\".")
  (:method (source name arguments &key context)
    (tool-permission-resources source name arguments :context context)))

;;; --- deciding a call ------------------------------------------------------------

(defun %context-get (context key)
  (and (consp context) (getf context key)))

(defun %approved-rules (agent context)
  "Saved rules (the store) then this run's \"always\" rules."
  (append (list-permission-rules (or (%context-get context :permission-store)
                                     (ai-agent-permission-store agent))
                                 :agent agent)
          (%context-get context :approved-permissions)))

(defgeneric agent-permission (agent source name arguments &key context)
  (:documentation "→ (values EFFECT RULE RESOURCES) for AGENT calling NAME on
SOURCE. EFFECT is :ALLOW, :ASK or :DENY. Rules are AI-AGENT-PERMISSIONS then
CONTEXT :permissions (run settings); the last match wins. No match → the
source's default (TOOL-NEEDS-APPROVAL-P → :ask). An :ask is then answered by
the saved rules (LIST-PERMISSION-RULES of CONTEXT :permission-store or
AI-AGENT-PERMISSION-STORE) and CONTEXT :approved-permissions (this run's
\"always\" replies), appended last; they never turn a deny into an allow.
Then CONTEXT :permission-policy and *PERMISSION-POLICY* may tighten it.")
  (:method ((agent ai-agent) source name arguments &key context)
    (let* ((resources (tool-permission-resources source name arguments
                                                 :context context))
           (rules (append (ai-agent-permissions agent)
                          (coerce-permission-rules
                           (%context-get context :permissions)))))
      (multiple-value-bind (effect rule)
          (permission-rules-effect rules name resources)
        (unless effect
          (setf effect (if (tool-needs-approval-p source name arguments
                                                  :context context)
                           :ask
                           :allow)))
        (when (eq effect :ask)
          (let ((approved (%approved-rules agent context)))
            (when approved
              (multiple-value-bind (answer answer-rule)
                  (permission-rules-effect (append rules approved) name resources)
                (when (member answer '(:allow :deny))
                  (setf effect answer rule answer-rule))))))
        (dolist (policy (list (%context-get context :permission-policy)
                              *permission-policy*))
          (multiple-value-bind (tight tight-rule)
              (permission-policy-effect policy name resources effect)
            (when (and tight (> (%effect-rank tight) (%effect-rank effect)))
              (setf effect tight rule tight-rule))))
        (values effect rule resources)))))

(defun %permission-context (run)
  (list :run run
        :permissions (%extra run :permissions)
        :permission-policy (%extra run :permission-policy)
        :permission-store (%extra run :permission-store)
        :approved-permissions (agent-run-approved-permissions run)))

(defun %run-permission-store (run)
  (or (%extra run :permission-store)
      (ai-agent-permission-store (agent-run-agent run))))

(defun %rule-list (rule)
  "RULE as the (action resource effect) list COERCE-PERMISSION-RULE reads."
  (list (permission-rule-action rule) (permission-rule-resource rule)
        (permission-rule-effect rule)))

(defun %add-approved-permissions (run rules)
  (dolist (rule rules)
    (unless (find rule (agent-run-approved-permissions run) :test #'%same-rule-p)
      (setf (agent-run-approved-permissions run)
            (append (agent-run-approved-permissions run) (list rule))))))

(defun %remember-approval (run inv)
  "The \"always\" reply for INV: one :allow rule per
TOOL-PERMISSION-ALWAYS-RESOURCES, added to the run and saved in its store.
→ the rules."
  (let* ((name (agent-invocation-name inv))
         (rules (mapcar (lambda (resource) (make-permission-rule name resource :allow))
                        (remove-duplicates
                         (tool-permission-always-resources
                          (agent-invocation-source inv) name
                          (agent-invocation-arguments inv)
                          :context (%permission-context run))
                         :test #'equal)))
         (store (%run-permission-store run)))
    (%add-approved-permissions run rules)
    (dolist (rule rules)
      (save-permission-rule store rule :agent (agent-run-agent run)))
    (%emit run :permission (list :invocation inv :rules rules))
    rules))

(defun %proposed-invocations (run)
  "RUN's invocations still waiting for an approval reply."
  (remove-if-not (lambda (i) (eq (agent-invocation-status i) :proposed))
                 (agent-run-invocations run)))

(defun %now-allowed-p (run inv)
  "T when INV, asked before, is now allowed (an \"always\" rule matches)."
  (eq :allow (agent-permission (agent-run-agent run) (agent-invocation-source inv)
                               (agent-invocation-name inv)
                               (agent-invocation-arguments inv)
                               :context (%permission-context run))))

(defun %permission-denied-p (run inv source)
  "T after recording INV as denied when the permission rules (or a policy)
deny it; the model gets a TOOL-PERMISSION-DENIED result."
  (multiple-value-bind (effect rule resources)
      (agent-permission (agent-run-agent run) source (agent-invocation-name inv)
                        (agent-invocation-arguments inv)
                        :context (%permission-context run))
    (when (eq effect :deny)
      (%refuse-invocation run inv
                          (make-condition 'tool-permission-denied
                                          :action (agent-invocation-name inv)
                                          :resources resources
                                          :rule rule
                                          :feedback (and rule (permission-rule-feedback rule))))
      t)))

(defun %refuse-invocation (run inv condition)
  "Record INV as not run because of CONDITION (a TOOL-PERMISSION-DENIED): the
model gets TOOL-FAILURE-TEXT as an error result."
  (setf (agent-invocation-status inv) :denied
        (agent-invocation-error-p inv) t
        (agent-invocation-result inv)
        (tool-failure-text condition (agent-invocation-name inv)))
  (when run
    (%emit run :permission (list :invocation inv :condition condition)))
  inv)

(defun %rejection (inv feedback)
  (make-condition 'tool-permission-denied
                  :decision :rejected
                  :action (agent-invocation-name inv)
                  :feedback feedback))
