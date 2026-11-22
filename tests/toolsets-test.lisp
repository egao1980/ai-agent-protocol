(in-package #:ai-agent-protocol/tests)

;;; 0.15.0 (#37): tool-source combinators (src/toolsets.lisp), rich tool
;;; output through the loop, policies as predicates (src/policy/).

(defun %ts-ft (name &optional (result name) &key approval)
  (make-function-tool :name name :description (format nil "tool ~a" name)
                      :approval-required-p approval
                      :handler (lambda (args) (declare (ignore args)) result)))

(defun %ts-src (&rest tools)
  "One source of TOOLS (a combined source of function tools)."
  (apply #'make-combined-tool-source tools))

(defun %ts-names (source &key context)
  (mapcar #'llm-tool-name (list-agent-tools source :context context)))

(defun %ts-call (source name &optional (arguments "{}"))
  "Invoke NAME on SOURCE under a loop → (values result error)."
  (with-agent-loop
    (handler-case (values (invoke-tool source name arguments) nil)
      (error (e) (values nil e)))))

(deftest wrapper-delegates
  (let ((w (make-wrapper-tool-source (%ts-src (%ts-ft "a") (%ts-ft "b" "B" :approval t)))))
    (ok (equal '("a" "b") (%ts-names w)))
    (ok (tool-executable-p w "a"))
    (ok (not (tool-executable-p w "zz")))
    (ok (tool-needs-approval-p w "b" "{}"))
    (ok (not (tool-needs-approval-p w "a" "{}")))
    (ok (equal "B" (%ts-call w "b")))))

(defclass counting-wrapper (wrapper-tool-source)
  ((calls :initform 0 :accessor counting-wrapper-calls)))

(defmethod invoke-tool-async :before ((source counting-wrapper) name arguments &key &allow-other-keys)
  (declare (ignore name arguments))
  (incf (counting-wrapper-calls source)))

(deftest wrapper-subclass-overrides-one-method
  (let ((w (make-instance 'counting-wrapper :inner (%ts-src (%ts-ft "a" "A")))))
    (ok (equal "A" (%ts-call w "a")))
    (ok (= 1 (counting-wrapper-calls w)))
    (ok (equal '("a") (%ts-names w)))))

(deftest combined-sources
  (let ((c (make-combined-tool-source (%ts-src (%ts-ft "a" "A1")) (%ts-ft "b" "B") (%ts-src (%ts-ft "c" "C")))))
    (ok (equal '("a" "b" "c") (%ts-names c)))
    (ok (tool-executable-p c "c"))
    (ok (not (tool-executable-p c "d")))
    (ok (equal "C" (%ts-call c "c")))
    (ok (typep (nth-value 1 (%ts-call c "d")) 'agent-unknown-tool))))

(deftest combined-duplicate-name
  (let ((c (make-combined-tool-source (%ts-src (%ts-ft "a" "first")) (%ts-src (%ts-ft "a" "second") (%ts-ft "b")))))
    (ok (signals (list-agent-tools c) 'tool-name-conflict))
    (handler-case (list-agent-tools c)
      (tool-name-conflict (e)
        (ok (equal "a" (tool-name-conflict-name e)))
        (ok (= 2 (length (tool-name-conflict-sources e))))))
    (handler-bind ((tool-name-conflict (lambda (e) (declare (ignore e)) (invoke-restart 'continue))))
      (ok (equal '("a" "b") (%ts-names c)))
      (ok (equal "first" (%ts-call c "a"))))))

(deftest prefixed-source
  (let ((p (make-prefixed-tool-source (%ts-src (%ts-ft "eval" "E") (%ts-ft "read" "R")) "mcp__lisp__")))
    (ok (equal '("mcp__lisp__eval" "mcp__lisp__read") (%ts-names p)))
    (ok (equal "tool eval" (llm-tool-description (first (list-agent-tools p)))))
    (ok (tool-executable-p p "mcp__lisp__eval"))
    (ok (not (tool-executable-p p "eval")))
    (ok (equal "R" (%ts-call p "mcp__lisp__read")))
    (ok (typep (nth-value 1 (%ts-call p "read")) 'agent-unknown-tool))))

(deftest renamed-source
  (let ((r (make-renamed-tool-source (%ts-src (%ts-ft "eval" "E") (%ts-ft "read" "R"))
                                     '(("lisp_eval" . "eval")))))
    (ok (equal '("lisp_eval" "read") (%ts-names r)))
    (ok (equal "E" (%ts-call r "lisp_eval")))
    (ok (equal "R" (%ts-call r "read")))
    (ok (not (tool-executable-p r "eval")))))

(deftest filtered-source-per-step
  (let* ((allowed '("a"))
         (f (make-filtered-tool-source
             (%ts-src (%ts-ft "a") (%ts-ft "b"))
             (lambda (context tool)
               (declare (ignore context))
               (member (llm-tool-name tool) allowed :test #'string=)))))
    (ok (equal '("a") (%ts-names f)))
    (ok (not (tool-executable-p f "b")))
    (setf allowed '("a" "b"))
    (ok (equal '("a" "b") (%ts-names f)))
    (ok (tool-executable-p f "b"))))

(deftest filtered-source-sees-context
  (let ((f (make-filtered-tool-source
            (%ts-src (%ts-ft "a") (%ts-ft "b"))
            (lambda (context tool)
              (or (equal (llm-tool-name tool) "a") (getf context :all))))))
    (ok (equal '("a") (%ts-names f :context '(:all nil))))
    (ok (equal '("a" "b") (%ts-names f :context '(:all t))))))

(deftest prepared-source
  (let ((p (make-prepared-tool-source
            (%ts-src (%ts-ft "a") (%ts-ft "b"))
            (lambda (context tools)
              (declare (ignore context))
              (mapcar (lambda (tool)
                        (make-llm-tool :name (llm-tool-name tool)
                                       :description "rewritten"
                                       :parameters (llm-tool-parameters tool)))
                      (remove "b" tools :key #'llm-tool-name :test #'string=))))))
    (ok (equal '("a") (%ts-names p)))
    (ok (equal "rewritten" (llm-tool-description (first (list-agent-tools p)))))
    (ok (not (tool-executable-p p "b")))))

(deftest prepared-source-may-not-add-or-rename
  (let ((p (make-prepared-tool-source
            (%ts-src (%ts-ft "a"))
            (lambda (context tools)
              (declare (ignore context tools))
              (list (make-llm-tool :name "z"))))))
    (ok (signals (list-agent-tools p) 'agent-error))))

(deftest approval-required-source
  (let ((s (make-approval-required-tool-source
            (%ts-src (%ts-ft "rm") (%ts-ft "ls") (%ts-ft "danger" "D" :approval t))
            (lambda (context tool arguments)
              (declare (ignore context))
              (and (equal (llm-tool-name tool) "rm")
                   (search "force" (or arguments "")))))))
    (ok (tool-needs-approval-p s "rm" "{\"force\":true}"))
    (ok (not (tool-needs-approval-p s "rm" "{}")))
    (ok (not (tool-needs-approval-p s "ls" "{\"force\":true}")))
    (ok (tool-needs-approval-p s "danger" "{}"))
    (ok (equal '("rm" "ls" "danger") (%ts-names s)))))

(deftest nesting-combinators
  (let ((s (make-combined-tool-source
            (make-prefixed-tool-source (%ts-src (%ts-ft "eval" "E1")) "mcp__one__")
            (make-prefixed-tool-source
             (make-filtered-tool-source (%ts-src (%ts-ft "eval" "E2") (%ts-ft "secret"))
                                        (lambda (c tool) (declare (ignore c))
                                          (not (equal (llm-tool-name tool) "secret"))))
             "mcp__two__"))))
    (ok (equal '("mcp__one__eval" "mcp__two__eval") (%ts-names s)))
    (ok (equal "E2" (%ts-call s "mcp__two__eval")))
    (ok (not (tool-executable-p s "mcp__two__secret")))))

;;; Policies as predicates (ai-agent-protocol/policy)

(deftest policy-as-filter-and-approval
  (let* ((pol (policy-protocol:make-policy
               :rules '((forbid :action "secret")
                        (ask :action "rm")
                        (permit :action "*"))))
         (f (make-filtered-tool-source (%ts-src (%ts-ft "ls") (%ts-ft "rm") (%ts-ft "secret")) pol))
         (a (make-approval-required-tool-source (%ts-src (%ts-ft "ls") (%ts-ft "rm")) pol)))
    (ok (equal '("ls" "rm") (%ts-names f)))
    (ok (tool-needs-approval-p a "rm" "{}"))
    (ok (not (tool-needs-approval-p a "ls" "{}")))))

(deftest policy-context-facts
  (let* ((pol (policy-protocol:make-policy
               :rules '((forbid :action "write" :when (mode plan))
                        (permit :action "*"))))
         (f (make-filtered-tool-source (%ts-src (%ts-ft "write") (%ts-ft "read")) pol)))
    (let ((ai-agent-protocol/policy:*tool-policy-context*
            (lambda (context tool arguments)
              (declare (ignore tool arguments))
              (and (getf context :plan) '((mode plan))))))
      (ok (equal '("read") (%ts-names f :context '(:plan t))))
      (ok (equal '("write" "read") (%ts-names f :context '(:plan nil)))))))

;;; Rich output through the loop

(deftest rich-tool-output-reaches-next-generate
  (with-agent-loop
    (let* ((seen nil)
           (image (make-llm-image-part :media-type "image/png" :data "iVBORw0KGgo="))
           (tool (make-function-tool
                  :name "shot" :async-p t
                  :handler (lambda (args ok err)
                             (declare (ignore args err))
                             (funcall ok (make-agent-tool-output
                                          :text "a screenshot" :parts (list image)
                                          :structured (let ((h (make-hash-table :test 'equal)))
                                                        (setf (gethash "w" h) 1) h))))))
           (fired nil)
           (backend (make-mock-llm-backend
                     :handler (lambda (backend turns &key &allow-other-keys)
                                (declare (ignore backend))
                                (cond (fired (setf seen turns)
                                             (make-llm-response
                                              :parts (list (make-llm-text-part :text "ok"))
                                              :finish-reason :stop))
                                      (t (setf fired t)
                                         (make-llm-response
                                          :parts (list (make-llm-tool-call-part
                                                        :id "c1" :name "shot" :arguments "{}"))
                                          :finish-reason :tool-use))))))
           (agent (make-ai-agent :name "rich" :backend backend :tools (list tool)))
           (run (run-ai-agent agent "go"))
           (inv (first (agent-run-invocations run))))
      (ok (eq :stop (agent-run-finish-reason run)))
      (ok (equal "a screenshot" (agent-invocation-result inv)))
      (ok (equal (list image) (agent-invocation-parts inv)))
      (ok (= 1 (gethash "w" (agent-invocation-structured inv))))
      (let* ((pos (position-if (lambda (turn)
                                 (some #'llm-tool-result-part-p (llm-turn-parts turn)))
                               seen))
             (next (and pos (nth (1+ pos) seen))))
        (ok pos)
        (ok (and next (eq :user (llm-turn-role next))))
        (ok (and next (equal "[output of tool shot (c1)]"
                             (llm-text-part-text (first (llm-turn-parts next))))))
        (ok (and next (eq image (second (llm-turn-parts next)))))))))

(deftest plain-string-output-unchanged
  (with-agent-loop
    (let* ((backend (make-mock-llm-backend
                     :handler (%one-shot-tools
                               (list (make-llm-tool-call-part :id "c1" :name "a" :arguments "{}"))
                               "done")))
           (agent (make-ai-agent :name "plain" :backend backend :tools (list (%ts-ft "a" "A"))))
           (run (run-ai-agent agent "go"))
           (inv (first (agent-run-invocations run))))
      (ok (equal "A" (agent-invocation-result inv)))
      (ok (null (agent-invocation-parts inv))))))
