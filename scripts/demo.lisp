;;;; Live LM Studio agent cycle: instructions + user prompt + CL tool.
;;;;
;;;;   set -a && source ../.env && set +a
;;;;   CL_SOURCE_REGISTRY="/Users/nikolaimatiushev/Projects/cl-workspace//:" \
;;;;     ros -l scripts/demo.lisp
;;;;
;;;; Needs LM Studio on OPENAI_BASE_URL (default http://127.0.0.1:1234/v1).

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&DEMO FAIL: ~A~%" c)
        (uiop:print-backtrace :condition c :stream *error-output*)
        (uiop:quit 1)))

(asdf:load-system "ai-agent-protocol")
(asdf:load-system "llm-protocol-openai")
(asdf:load-system "event-backend-libuv")
(asdf:load-system "http-backend-async")
(asdf:load-system "json-backend-jzon")

(in-package #:cl-user)

(defun %ht (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k h) v))
    h))

(defun %num (x)
  (cond
    ((realp x) x)
    ((stringp x) (let ((n (ignore-errors (read-from-string x))))
                   (if (realp n) n (error "not a number: ~s" x))))
    (t (error "not a number: ~s" x))))

(defun %decode-args (args)
  (cond
    ((hash-table-p args) args)
    ((and (stringp args) (plusp (length (string-trim '(#\Space #\Tab #\Newline) args))))
     (json-protocol:decode args))
    (t (%ht))))

(defun add-handler (args)
  (let* ((obj (%decode-args args))
         (a (%num (or (gethash "a" obj) (gethash "x" obj))))
         (b (%num (or (gethash "b" obj) (gethash "y" obj))))
         (sum (+ a b)))
    (format t "~&   [CL] add a=~a b=~a → ~a~%" a b sum)
    (princ-to-string sum)))

(defun on-event (kind payload)
  (case kind
    (:started
     (format t "~&== started agent=~s~%"
             (ai-agent-protocol:ai-agent-name
              (ai-agent-protocol:agent-run-agent payload))))
    (:step
     (format t "~&-- generate step ~a~%" payload))
    (:response
     (format t "~&   llm finish=~s text=~s calls=~s~%"
             (llm-protocol:llm-response-finish-reason payload)
             (llm-protocol:llm-response-text payload)
             (mapcar #'llm-protocol:llm-tool-call-part-name
                     (llm-protocol:llm-response-tool-calls payload))))
    (:finished
     (format t "~&== finished ~s~%"
             (ai-agent-protocol:agent-run-finish-reason payload)))
    (t
     (format t "~&   event ~s~%" kind))))

(defun run-demo ()
  (setf http-backend-async:*event-backend-maker*
        #'event-backend-libuv:make-libuv-backend)
  (setf http-protocol:*http-backend*
        (http-backend-async:make-async-backend))
  (let* ((backend (llm-protocol-openai:make-openai-compat-backend))
         (params (%ht "type" "object"
                      "properties"
                      (%ht "a" (%ht "type" "number" "description" "first addend")
                           "b" (%ht "type" "number" "description" "second addend"))
                      "required" (vector "a" "b")))
         (agent (ai-agent-protocol:make-ai-agent
                 :name "desk-calc"
                 :backend backend
                 :instructions
                 "You are a desk calculator. For any arithmetic you MUST call the add tool.
Never add numbers yourself. After the tool returns, answer in one short sentence."))
         (user "What is 17 plus 25?")
         (eb (event-backend-libuv:make-libuv-backend))
         (el (event-protocol:make-event-loop eb)))
    (ai-agent-protocol:define-agent-tool
        agent "add"
        (:description "Add two numbers and return the sum."
         :parameters params)
        (args)
      (add-handler args))
    (format t "~&demo: base=~a model=~s~%"
            (llm-protocol-openai:openai-base-url backend)
            (llm-protocol-openai:openai-default-model backend))
    (format t "~&demo: instructions set, user=~s, tool=add~%" user)
    (event-protocol:with-event-backend (eb)
      (event-protocol:with-event-loop-var (el)
        (let ((run (ai-agent-protocol:run-ai-agent
                    agent user
                    :settings (ai-agent-protocol:make-agent-settings
                               :llm (llm-protocol:make-llm-settings
                                     :temperature 0 :max-tokens 256)
                               :max-steps 4
                               :extra '(:first-tool-choice :required))
                    :on-event #'on-event)))
          (format t "~&demo: text=~s~%" (ai-agent-protocol:agent-run-text run))
          (dolist (inv (ai-agent-protocol:agent-run-invocations run))
            (format t "~&demo: invocation ~s status=~s args=~s result=~s~%"
                    (ai-agent-protocol:agent-invocation-name inv)
                    (ai-agent-protocol:agent-invocation-status inv)
                    (ai-agent-protocol:agent-invocation-arguments inv)
                    (ai-agent-protocol:agent-invocation-result inv)))
          (unless (find :done (ai-agent-protocol:agent-run-invocations run)
                        :key #'ai-agent-protocol:agent-invocation-status)
            (error "expected a completed CL tool invocation"))
          (unless (eq :stop (ai-agent-protocol:agent-run-finish-reason run))
            (error "expected finish :stop, got ~s"
                   (ai-agent-protocol:agent-run-finish-reason run)))
          run)))))

(let ((run (run-demo)))
  (declare (ignore run))
  (format t "~&DEMO OK~%")
  (uiop:quit 0))
