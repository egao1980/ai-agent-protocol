(in-package #:ai-agent-protocol/tests)

;;; ai-agent-protocol/mcp 0.2.0 (#37): MCP-TOOL-SOURCE over a real client
;;; transport (rpc-backend-stdio's in-memory pipe pair, wired with
;;; mcp-protocol 0.4.0's MCP-CLIENT-TRANSPORT-HANDLERS). Bounded waits.

(defun %jo (&rest kvs) (apply #'mcp-protocol:json-object kvs))

(defclass counting-server (mcp-protocol:mcp-server)
  ((lists :initform 0 :accessor counting-server-lists)
   (seen-args :initform nil :accessor counting-server-seen-args)))

(defmethod mcp-protocol:list-tools :before ((server counting-server) &key cursor)
  (declare (ignore cursor))
  (incf (counting-server-lists server)))

(defun %content (&rest items) (coerce items 'vector))

(defun %fake-mcp-server ()
  (let ((server (make-instance 'counting-server :name "fake" :version "1.0.0")))
    (labels ((tool (&rest args)
               (mcp-protocol:register-tool server (apply #'mcp-protocol:make-mcp-tool args)))
             (result (content &rest more)
               (apply #'%jo "content" content more)))
      (tool "echo" :description "echoes msg" :input-schema (%jo "type" "object")
                   :annotations (%jo "readOnlyHint" t)
                   :output-schema (%jo "type" "object")
                   :handler (lambda (args)
                              (setf (counting-server-seen-args server) args)
                              (result (%content (%jo "type" "text"
                                                     "text" (or (mcp-protocol:param args "msg") ""))))))
      (tool "titled" :title "A titled tool" :description ""
                     :handler (lambda (args) (declare (ignore args)) "t"))
      (tool "app_only" :description "app" :meta (%jo "ui" (%jo "visibility" (vector "app")))
                       :handler (lambda (args) (declare (ignore args)) "app"))
      (tool "multi" :description "two texts and an image" :input-schema (%jo "type" "object")
                    :handler (lambda (args) (declare (ignore args))
                               (result (%content (%jo "type" "text" "text" "one")
                                                 (%jo "type" "image" "mimeType" "image/png"
                                                      "data" "iVBORw0KGgo=")
                                                 (%jo "type" "text" "text" "two")))))
      (tool "structured" :description "structured only" :input-schema (%jo "type" "object")
                         :handler (lambda (args) (declare (ignore args))
                                    (result (vector) "structuredContent" (%jo "n" 1))))
      (tool "fails" :description "isError" :input-schema (%jo "type" "object")
                    :handler (lambda (args) (declare (ignore args))
                               (result (%content (%jo "type" "text" "text" "disk full"))
                                       "isError" t)))
      (tool "slow" :description "sleeps" :input-schema (%jo "type" "object")
                   :handler (lambda (args) (declare (ignore args)) (sleep 3) "late"))
      (tool "grow" :description "adds a tool and says so" :input-schema (%jo "type" "object")
                   :handler (lambda (args) (declare (ignore args))
                              (tool "grown" :description "new" :handler (lambda (a) (declare (ignore a)) "g"))
                              (mcp-protocol:notify-tools-list-changed server)
                              "grew")))
    server))

(defmacro with-fake-mcp ((client server) &body body)
  (let ((st (gensym)) (ct (gensym)) (thread (gensym)) (n (gensym)) (r (gensym)))
    `(let* ((,server (%fake-mcp-server))
            (,client (make-instance 'mcp-protocol:mcp-client :name "host" :version "0.1.0")))
       (multiple-value-bind (,n ,r) (mcp-protocol:mcp-client-transport-handlers ,client)
         (multiple-value-bind (,st ,ct)
             (rpc-backend-stdio:make-stdio-rpc-pair :on-notification ,n :on-request ,r
                                                    :dispatch :concurrent)
           (setf (mcp-protocol:mcp-client-transport ,client) ,ct
                 (mcp-protocol:mcp-server-notifier ,server)
                 (lambda (m p) (rpc-protocol:rpc-notify m p :transport ,st)))
           (let ((,thread (bt2:make-thread
                           (lambda () (ignore-errors (mcp-protocol:serve-mcp ,server :transport ,st)))
                           :name "fake mcp server")))
             (unwind-protect (progn (mcp-protocol:mcp-initialize ,client) ,@body)
               (rpc-backend-stdio:close-stdio-rpc-transport ,ct :force t)
               (ignore-errors (bt2:join-thread ,thread)))))))))

(defun %wait-for (fn &key (timeout 5))
  (loop with end = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        for v = (funcall fn) when v return v
        when (> (get-internal-real-time) end) return nil
        do (sleep 0.02)))

(defun %invoke (source name &optional (arguments "{}"))
  "→ (values result condition) of INVOKE-TOOL under a loop."
  (with-agent-loop
    (handler-case (values (invoke-tool source name arguments) nil)
      (error (e) (values nil e)))))

(deftest mcp-source-tool-error-behavior-checked
  (ok (signals (ai-agent-protocol/mcp:make-mcp-tool-source nil :tool-error-behavior :explode)
               'type-error))
  (ok (equal '(:retry :failed :error) ai-agent-protocol/mcp:+tool-error-behaviors+)))

(deftest mcp-source-lists-tools
  (with-fake-mcp (client server)
    (let* ((src (ai-agent-protocol/mcp:make-mcp-tool-source client))
           (tools (list-agent-tools src))
           (names (mapcar #'llm-tool-name tools)))
      (ok (equal '("echo" "titled" "multi" "structured" "fails" "slow" "grow") names))
      (ok (not (member "app_only" names :test #'string=)))
      (ok (not (tool-executable-p src "app_only")))
      (ok (equal "A titled tool" (llm-tool-description (find "titled" tools :key #'llm-tool-name :test #'string=))))
      (ok (hash-table-p (llm-tool-parameters (find "titled" tools :key #'llm-tool-name :test #'string=))))
      (ok (tool-executable-p src "echo"))
      (ok (not (tool-executable-p src "nope"))))))

(deftest mcp-source-tool-metadata
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client)))
      (ok (typep (ai-agent-protocol/mcp:find-mcp-tool src "echo") 'mcp-protocol:mcp-tool))
      (ok (null (ai-agent-protocol/mcp:find-mcp-tool src "nope")))
      (ok (ai-agent-protocol/mcp:mcp-tool-source-read-only-p src "echo"))
      (ok (not (ai-agent-protocol/mcp:mcp-tool-source-read-only-p src "multi")))
      (ok (not (ai-agent-protocol/mcp:mcp-tool-source-read-only-p src "nope")))
      (ok (hash-table-p (ai-agent-protocol/mcp:mcp-tool-source-output-schema src "echo")))
      (ok (null (ai-agent-protocol/mcp:mcp-tool-source-output-schema src "multi"))))))

(deftest mcp-source-caches-the-list
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client)))
      (list-agent-tools src)
      (list-agent-tools src)
      (tool-executable-p src "echo")
      (ai-agent-protocol/mcp:find-mcp-tool src "echo")
      (ok (= 1 (counting-server-lists server)))
      (ai-agent-protocol/mcp:mcp-tool-source-invalidate src)
      (list-agent-tools src)
      (ok (= 2 (counting-server-lists server))))))

(deftest mcp-source-refreshes-on-list-changed
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client)))
      (ok (not (tool-executable-p src "grown")))
      (ok (equal "grew" (%invoke src "grow")))
      (ok (%wait-for (lambda () (plusp (mcp-protocol:mcp-client-tools-generation client)))))
      (ok (tool-executable-p src "grown"))
      (ok (= 2 (counting-server-lists server))))))

(deftest mcp-source-without-cache
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client :cache-tools nil)))
      (list-agent-tools src)
      (list-agent-tools src)
      (ok (= 2 (counting-server-lists server))))))

(deftest mcp-source-text-result
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client)))
      (ok (equal "hi" (%invoke src "echo" "{\"msg\":\"hi\"}")))
      (ok (equal "" (%invoke src "echo" "")))
      (ok (typep (nth-value 1 (%invoke src "echo" "[1,2]")) 'agent-error)))))

(deftest mcp-source-image-result
  (with-fake-mcp (client server)
    (let* ((src (ai-agent-protocol/mcp:make-mcp-tool-source client))
           (out (%invoke src "multi")))
      (ok (typep out 'agent-tool-output))
      (ok (equal (format nil "one~%[image image/png]~%two") (agent-tool-output-text out)))
      (ok (= 1 (length (agent-tool-output-parts out))))
      (let ((part (first (agent-tool-output-parts out))))
        (ok (llm-image-part-p part))
        (ok (equal "image/png" (llm-image-part-media-type part)))
        (ok (equal "iVBORw0KGgo=" (llm-image-part-data part)))))))

(deftest mcp-source-structured-result
  (with-fake-mcp (client server)
    (let* ((src (ai-agent-protocol/mcp:make-mcp-tool-source client))
           (out (%invoke src "structured")))
      (ok (typep out 'agent-tool-output))
      (ok (equal "{\"n\":1}" (agent-tool-output-text out)))
      (ok (= 1 (gethash "n" (agent-tool-output-structured out)))))))

(deftest-parametrize mcp-source-is-error
    ((behavior type retryable)
     (:retry ai-agent-protocol/mcp:mcp-tool-result-error t)
     (:failed ai-agent-protocol/mcp:mcp-tool-result-error nil)
     (:error ai-agent-protocol/mcp:mcp-tool-run-error nil))
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client :tool-error-behavior behavior)))
      (multiple-value-bind (result err) (%invoke src "fails")
        (ok (null result))
        (ok (typep err type))
        (ok (search "disk full" (princ-to-string err)))
        (when (typep err 'ai-agent-protocol/mcp:mcp-tool-result-error)
          (ok (eq retryable (ai-agent-protocol/mcp:mcp-tool-result-error-retryable err))))))))

(deftest mcp-source-transport-error-is-not-a-tool-error
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client)))
      (list-agent-tools src)
      (rpc-backend-stdio:close-stdio-rpc-transport (mcp-protocol:mcp-client-transport client) :force t)
      (multiple-value-bind (result err) (%invoke src "echo")
        (ok (null result))
        (ok (typep err '(or mcp-protocol:mcp-error rpc-protocol:rpc-error)))
        (ok (not (typep err 'ai-agent-protocol/mcp:mcp-tool-result-error)))))))

(deftest mcp-source-timeout
  (with-fake-mcp (client server)
    (let ((src (ai-agent-protocol/mcp:make-mcp-tool-source client :timeout 0.5)))
      (ok (typep (nth-value 1 (%invoke src "slow")) 'mcp-protocol:mcp-timeout)))))

(deftest mcp-source-process-tool-call
  (with-fake-mcp (client server)
    (let* ((seen nil)
           (src (ai-agent-protocol/mcp:make-mcp-tool-source
                 client
                 :process-tool-call (lambda (context call name args)
                                      (setf seen (list context name))
                                      (setf (gethash "msg" args) "rewritten")
                                      (funcall call name args)))))
      (ok (equal "rewritten" (%invoke src "echo" "{\"msg\":\"orig\"}")))
      (ok (equal "echo" (second seen)))
      (ok (equal "rewritten" (gethash "msg" (counting-server-seen-args server)))))))

(deftest-parametrize mcp-result-output-mapping
    ((content extra text nparts)
     ((vector (%jo "type" "text" "text" "a")) nil "a" 0)
     ((vector (%jo "type" "text" "text" "a") (%jo "type" "text" "text" "b")) nil (format nil "a~%b") 0)
     ((vector (%jo "type" "audio" "mimeType" "audio/wav" "data" "AA==")) nil "[audio audio/wav]" 0)
     ((vector (%jo "type" "resource" "resource" (%jo "uri" "file:///a" "text" "body"))) nil "body" 0)
     ((vector (%jo "type" "resource" "resource" (%jo "uri" "file:///b" "mimeType" "application/pdf" "blob" "AA==")))
      nil "[resource file:///b application/pdf]" 0)
     ((vector (%jo "type" "resource_link" "uri" "file:///c" "name" "c")) nil "[resource file:///c]" 0)
     ((vector (%jo "type" "image" "mimeType" "image/jpeg" "data" "AA==")) nil "[image image/jpeg]" 1)
     ((vector) nil "" 0))
  (let ((out (ai-agent-protocol/mcp:mcp-result-output (apply #'%jo "content" content extra))))
    (ok (typep out 'agent-tool-output))
    (ok (equal text (agent-tool-output-text out)))
    (ok (= nparts (length (agent-tool-output-parts out))))))

(deftest mcp-source-through-the-agent-prefixed
  (with-fake-mcp (client server)
    (with-agent-loop
      (let* ((src (make-prefixed-tool-source (ai-agent-protocol/mcp:make-mcp-tool-source client)
                                             "mcp__fake__"))
             (offered nil)
             (fired nil)
             (backend (make-mock-llm-backend
                       :handler (lambda (backend turns &key tools &allow-other-keys)
                                  (declare (ignore backend turns))
                                  (unless offered (setf offered (mapcar #'llm-tool-name tools)))
                                  (if fired
                                      (make-llm-response :parts (list (make-llm-text-part :text "done"))
                                                         :finish-reason :stop)
                                      (progn (setf fired t)
                                             (make-llm-response
                                              :parts (list (make-llm-tool-call-part
                                                            :id "c1" :name "mcp__fake__echo"
                                                            :arguments "{\"msg\":\"via agent\"}"))
                                              :finish-reason :tool-use))))))
             (agent (make-ai-agent :name "host" :backend backend :tools (list src)))
             (run (run-ai-agent agent "go"))
             (inv (first (agent-run-invocations run))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (member "mcp__fake__echo" offered :test #'string=))
        (ok (not (member "mcp__fake__app_only" offered :test #'string=)))
        (ok (equal "via agent" (agent-invocation-result inv)))))))

(deftest mcp-source-error-result-through-the-agent
  (with-fake-mcp (client server)
    (with-agent-loop
      (let* ((src (ai-agent-protocol/mcp:make-mcp-tool-source client))
             (backend (make-mock-llm-backend
                       :handler (%one-shot-tools
                                 (list (make-llm-tool-call-part :id "c1" :name "fails" :arguments "{}"))
                                 "noted")))
             (agent (make-ai-agent :name "host" :backend backend :tools (list src)))
             (run (with-auto-skip-tool (run-ai-agent agent "go")))
             (inv (first (agent-run-invocations run))))
        (ok (eq :stop (agent-run-finish-reason run)))
        (ok (agent-invocation-error-p inv))
        (ok (search "disk full" (agent-invocation-result inv)))))))
