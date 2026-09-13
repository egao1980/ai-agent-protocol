(in-package #:ai-agent-protocol/durability)

;;; Optional :durability spine. Core stays free of task-protocol; RUN-AI-AGENT
;;; soft-calls these symbols when the subsystem is loaded.

(defclass agent-durability ()
  ((journal :initarg :journal :accessor agent-durability-journal)
   (task :initarg :task :accessor agent-durability-task)
   (consumed :initform nil :accessor agent-durability-consumed)
   (bound-p :initform nil :accessor agent-durability-bound-p)))

(defun agent-durability-p (x)
  (typep x 'agent-durability))

(defun make-agent-durability (&key journal task task-id)
  (let* ((journal (or journal (make-in-memory-journal)))
         (task (or task (make-durable-task
                         :id (or task-id
                                 (format nil "agent-~d" (get-universal-time)))
                         :journal journal))))
    (setf (durable-task-journal task) journal)
    (make-instance 'agent-durability :journal journal :task task)))

(defun %normalize-step-name (name)
  (if (stringp name) name (string-downcase (string name))))

(defun bind-agent-durability (durability)
  "Replay JOURNAL into TASK once. Subsequent calls are no-ops."
  (check-type durability agent-durability)
  (unless (agent-durability-bound-p durability)
    (with-durable-task ((agent-durability-task durability)
                        (agent-durability-journal durability))
      (setf (agent-durability-consumed durability)
            task-protocol::*consumed-steps*))
    (setf (agent-durability-bound-p durability) t))
  durability)

(defun coerce-agent-durability (x)
  "Accept AGENT-DURABILITY, a journal, a DURABLE-TASK, T, or
   (:journal J :task T :task-id ID)."
  (cond
    ((null x) nil)
    ((agent-durability-p x)
     (bind-agent-durability x))
    ((durable-task-p x)
     (bind-agent-durability
      (make-agent-durability :task x
                             :journal (or (durable-task-journal x)
                                          (make-in-memory-journal)))))
    ((and (consp x) (keywordp (car x)))
     (bind-agent-durability
      (make-agent-durability :journal (getf x :journal)
                             :task (getf x :task)
                             :task-id (getf x :task-id))))
    ((eq x t)
     (bind-agent-durability (make-agent-durability)))
    (t
     (bind-agent-durability (make-agent-durability :journal x)))))

(defun call-with-agent-durability (durability thunk)
  (let ((*journal* (agent-durability-journal durability))
        (*task* (agent-durability-task durability))
        (task-protocol::*consumed-steps* (agent-durability-consumed durability)))
    (unwind-protect (funcall thunk)
      (setf (agent-durability-consumed durability)
            task-protocol::*consumed-steps*))))

(defun %find-exact-step (journal task name idempotency-key)
  "Match NAME + IDEMPOTENCY-KEY exactly. Do not fall back to name-only —
   generate/2 must not reuse generate/1."
  (find-if (lambda (e)
             (and (typep e 'step-completed)
                  (equal name (step-name e))
                  (equal idempotency-key (step-idempotency-key e))))
           (journal-events journal task)))

(defun call-durable-step (durability name thunk &key idempotency-key)
  "Execute THUNK once and journal it. On resume, return the recorded result."
  (multiple-value-bind (hit result)
      (durable-step-replayed durability name :idempotency-key idempotency-key)
    (if hit
        result
        (record-durable-step durability name (funcall thunk)
                             :idempotency-key idempotency-key))))

(defun durable-step-replayed (durability name &key idempotency-key)
  "If NAME/KEY is already journaled, consume it and return (values T result)."
  (call-with-agent-durability
   durability
   (lambda ()
     (let* ((name (%normalize-step-name name))
            (recorded (%find-exact-step (agent-durability-journal durability)
                                        (agent-durability-task durability)
                                        name idempotency-key)))
       (when recorded
         (push recorded task-protocol::*consumed-steps*)
         (values t (step-result recorded)))))))

(defun record-durable-step (durability name result &key idempotency-key)
  "Journal RESULT as a completed step (or return the recorded value)."
  (call-with-agent-durability
   durability
   (lambda ()
     (let* ((name (%normalize-step-name name))
            (task (agent-durability-task durability))
            (journal (agent-durability-journal durability))
            (existing (%find-exact-step journal task name idempotency-key)))
       (cond
         (existing
          (push existing task-protocol::*consumed-steps*)
          (step-result existing))
         (t
          (let ((event (make-instance 'step-completed
                                      :task-id (durable-task-id task)
                                      :name name
                                      :result (canonicalize-value result)
                                      :idempotency-key idempotency-key)))
            (append-event journal event)
            (apply-event task event)
            (push event task-protocol::*consumed-steps*)
            (step-result event))))))))

(defun durable-wait-input (durability &key prompt)
  "Journal WAIT-INPUT once. Replay is a no-op if one is already present."
  (call-with-agent-durability
   durability
   (lambda ()
     (let* ((task (agent-durability-task durability))
            (journal (agent-durability-journal durability))
            (already (find-if (lambda (e) (typep e 'wait-input))
                              (journal-events journal task))))
       (if already
           task
           (request-input task :prompt prompt))))))
