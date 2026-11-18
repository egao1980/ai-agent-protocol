(in-package #:ai-agent-protocol)

;;; Lazily discovered, path-scoped instructions (steer-protocol 0.3).
;;;
;;; Configure with settings extra :instructions-discovery (a
;;; STEER:INSTRUCTION-DISCOVERY, a project root, or a plist for
;;; STEER:COERCE-INSTRUCTION-DISCOVERY; NIL turns it off), or give the agent an
;;; instruction-discovery as :steering. Each run works on its own copy.
;;;
;;; Before each generate the loop takes the paths that finished tool
;;; invocations named (TOOL-INSTRUCTION-PATHS), asks the discovery for
;;; instruction files not yet in context, and appends them as one turn
;;; (AGENT-INSTRUCTIONS-TURN). A :compacted event forgets what was injected and
;;; re-injects it before the next generate.

(defparameter *instruction-path-keys*
  '("path" "file_path" "filePath" "file" "filename" "paths" "files"
    "directory" "dir" "cwd" "notebook_path")
  "Tool argument keys whose string values name paths, for the default
TOOL-INSTRUCTION-PATHS.")

(defgeneric tool-instruction-paths (source name arguments &key context)
  (:documentation "→ list of path strings a call of NAME on SOURCE with
ARGUMENTS reads or touches, for instruction discovery. Default: the
TOOL-PERMISSION-RESOURCES that look like paths, plus string values of
*INSTRUCTION-PATH-KEYS* in ARGUMENTS (a JSON object string, plist or hash
table).")
  (:method (source name arguments &key context)
    (remove-duplicates
     (remove-if-not #'%path-like-p
                    (append (tool-permission-resources source name arguments
                                                       :context context)
                            (%argument-paths arguments)))
     :test #'equal :from-end t)))

(defun %path-like-p (s)
  (and (stringp s)
       (plusp (length s))
       (string/= s "*")
       (not (search "://" s))
       (notany (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) s)))

(defun %path-key-p (key)
  (member (substitute #\_ #\- (string key)) *instruction-path-keys*
          :test #'string-equal))

(defun %string-values (v)
  (cond ((stringp v) (list v))
        ((and (consp v) (every #'stringp v)) (copy-list v))
        ((and (vectorp v) (every #'stringp v)) (coerce v 'list))
        (t nil)))

(defun %argument-paths (arguments)
  (typecase arguments
    (string (%json-path-values arguments))
    (hash-table (loop for k being the hash-keys of arguments using (hash-value v)
                      when (and (or (stringp k) (symbolp k)) (%path-key-p k))
                        append (%string-values v)))
    (cons (and (evenp (length arguments))
               (loop for (k v) on arguments by #'cddr
                     when (and (symbolp k) (%path-key-p k))
                       append (%string-values v))))
    (t nil)))

;;; Shallow scan of a JSON object string: "key": "value" or "key": ["a", "b"]
;;; for keys in *INSTRUCTION-PATH-KEYS*. Malformed input yields what was found
;;; before the problem; it is a hint, not a parser.

(defun %json-string-at (s i)
  "S[I] is a double quote → (values string next-index) or NIL."
  (let ((n (length s)))
    (when (and (< i n) (char= (char s i) #\"))
      (with-output-to-string (o)
        (loop with j = (1+ i)
              while (< j n)
              do (let ((c (char s j)))
                   (cond ((char= c #\")
                          (return-from %json-string-at
                            (values (get-output-stream-string o) (1+ j))))
                         ((and (char= c #\\) (< (1+ j) n))
                          (let ((e (char s (1+ j))))
                            (write-char (case e (#\n #\Newline) (#\t #\Tab) (t e)) o))
                          (incf j 2))
                         (t (write-char c o) (incf j)))))
        (return-from %json-string-at nil)))))

(defun %skip-ws (s i)
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) s
                       :start (min i (length s)))
      (length s)))

(defun %json-path-values (s)
  (let ((out '()) (i 0) (n (length s)))
    (loop
      (let ((q (position #\" s :start (min i n))))
        (unless q (return))
        (multiple-value-bind (key next) (%json-string-at s q)
          (unless key (return))
          (let ((j (%skip-ws s next)))
            (cond
              ((and (< j n) (char= (char s j) #\:) (%path-key-p key))
               (let ((k (%skip-ws s (1+ j))))
                 (cond
                   ((and (< k n) (char= (char s k) #\"))
                    (multiple-value-bind (v after) (%json-string-at s k)
                      (unless v (return))
                      (push v out)
                      (setf i after)))
                   ((and (< k n) (char= (char s k) #\[))
                    (let ((m (%skip-ws s (1+ k))))
                      (loop while (and (< m n) (char= (char s m) #\"))
                            do (multiple-value-bind (v after) (%json-string-at s m)
                                 (unless v (return))
                                 (push v out)
                                 (setf m (%skip-ws s after))
                                 (when (and (< m n) (char= (char s m) #\,))
                                   (setf m (%skip-ws s (1+ m))))))
                      (setf i m)))
                   (t (setf i k)))))
              (t (setf i next)))))))
    (nreverse out)))

;;; --- configuration ----------------------------------------------------------

(defun %configured-discovery (agent settings)
  (let* ((extra (agent-settings-extra settings))
         (cfg (if (and (listp extra) (member :instructions-discovery extra))
                  (getf extra :instructions-discovery)
                  (let ((steer (ai-agent-steering agent)))
                    (and (typep steer 'steer:instruction-discovery) steer)))))
    (cond ((null cfg) nil)
          ((eq cfg t) (steer:make-instruction-discovery (uiop:getcwd)))
          (t (steer:coerce-instruction-discovery cfg)))))

(defun %init-run-instructions (run)
  (let ((disc (%configured-discovery (agent-run-agent run) (agent-run-settings run))))
    (setf (agent-run-instructions run)
          (and disc (steer:copy-instruction-discovery disc)))))

;;; --- injection --------------------------------------------------------------

(defgeneric agent-instructions-turn (agent directives &key run)
  (:documentation "The turn that carries newly discovered instruction
DIRECTIVES into the conversation. Default: a user turn of
STEER:COMPILE-INSTRUCTIONS.")
  (:method ((agent ai-agent) directives &key run)
    (declare (ignore run))
    (user-turn (steer:compile-instructions directives))))

(defun %new-invocation-paths (run)
  (let ((paths '()))
    (dolist (inv (agent-run-invocations run))
      (when (and (member (agent-invocation-status inv) '(:done :error) :test #'eq)
                 (not (member inv (agent-run-instructions-scanned run) :test #'eq)))
        (push inv (agent-run-instructions-scanned run))
        (let ((source (agent-invocation-source inv)))
          (unless (typep source 'ai-agent) ; handoffs touch no files
            (setf paths
                  (append paths
                          (tool-instruction-paths source
                                                  (agent-invocation-name inv)
                                                  (agent-invocation-arguments inv)
                                                  :context (list :run run))))))))
    paths))

(defun %inject-instructions (run)
  "Append a turn with instructions the run's tool calls reached (and any a
compaction dropped). → the new directives."
  (let ((disc (agent-run-instructions run)))
    (when disc
      (let* ((paths (append (shiftf (agent-run-instructions-reinject run) nil)
                            (%new-invocation-paths run)))
             (new (and paths (steer:discover-instructions disc paths))))
        (when new
          (setf (agent-run-turns run)
                (append (agent-run-turns run)
                        (list (agent-instructions-turn (agent-run-agent run) new
                                                       :run run))))
          (%emit run :instructions new))
        new))))

(defun %instructions-compacted (run)
  "Compaction dropped injected turns: forget them and queue re-injection."
  (let ((disc (agent-run-instructions run)))
    (when disc
      (setf (agent-run-instructions-reinject run)
            (append (agent-run-instructions-reinject run)
                    (mapcar #'steer:steer-directive-path (steer:list-directives disc))))
      (steer:forget-instructions disc))))
