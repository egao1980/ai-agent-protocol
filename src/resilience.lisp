(in-package #:ai-agent-protocol)

;;; 0.16.0 — resilience in the loop (cl-stack-code H3f). After 0.15.0 (#37).
;;;
;;; The loop already retries a failed generate by llm-protocol's
;;; RETRY-POLICY (settings extra :RETRY-POLICY, default *RETRY-POLICY*),
;;; discards the in-flight partial turn, compacts once on
;;; LLM-CONTEXT-OVERFLOW, and compacts before a step over :COMPACT-AT. This
;;; slice closes four gaps a product hits:
;;;
;;; 1. CANCEL DURING A BACKOFF. 0.15 waits out the whole delay (EVENT:SLEEP*)
;;;    before it notices a cancel, so Esc during "retrying in 30 s" does
;;;    nothing for 30 s. Now CANCEL-AGENT-RUN on a run waiting to retry
;;;    stops the timer and finishes the run :CANCELED on the next loop turn
;;;    (the usual AGENT-CANCELED signal and its CONTINUE / ABORT restarts;
;;;    CONTINUE retries at once). No generate is sent after the cancel.
;;;
;;; 2. RICHER EVENTS.
;;;    :RETRY payload: (:reason CLASS :attempt N :delay S :condition C
;;;      :max-attempts M :status STATUS-OR-NIL :retry-after S-OR-NIL). REASON
;;;      is LLM-RETRY-CLASS (:rate-limit :overloaded :transient), or
;;;      :CONTEXT-OVERFLOW (the compact-and-retry), or :OUTPUT-TRUNCATED
;;;      (rule 4). ATTEMPT is the attempt that failed (1-based). M is the
;;;      policy's max attempts (NIL for the two non-policy reasons).
;;;    :COMPACTED payload: (:before N :after M :reason R :summary TURNS
;;;      :keep K :tokens-before T). R is :THRESHOLD (before a step),
;;;      :OVERFLOW (after LLM-CONTEXT-OVERFLOW) or :MANUAL (COMPACT-AGENT-RUN).
;;;      SUMMARY is the list of new turns placed before the kept tail (what
;;;      %COMPACTION-RECORD calls :HEAD); K is the number of turns kept from
;;;      the end of the old list, by identity; T the anchored token count
;;;      that triggered it (NIL when forced). A product can rebuild the
;;;      compacted list from its own full history with SUMMARY + the last K.
;;;
;;; 3. OUTPUT CONTINUATION. Settings extra :MAX-CONTINUATIONS (integer ≥ 0,
;;;    default 0 = 0.15 behaviour). A response with finish reason :LENGTH
;;;    and no tool calls, while fewer than N continuations ran in this run
;;;    since the last response that did not end :LENGTH: the assistant turn
;;;    stays in history, a user turn (CONTINUATION-TURN) is appended, and the
;;;    loop takes one more step (it counts against max steps). Event
;;;    (:continuation (:step S :count K :max N)). Past N the run finishes
;;;    :LENGTH as before. CONTINUATION-TURN &optional prompt → a :USER turn
;;;    whose only part is a text part with PROMPT (default
;;;    +CONTINUATION-PROMPT+; the loop uses settings extra
;;;    :CONTINUATION-PROMPT when set). CONTINUATION-TURN-P turn &optional
;;;    prompt → T for exactly such a turn (role :USER, one text part, text
;;;    STRING= PROMPT), so a transcript or a session record can hide it; the
;;;    test is on content, so it survives TURN-STATE round trips. AGENT-RUN-TEXT of a continued run is
;;;    the concatenation of the continued assistant texts (no separator),
;;;    from the first of the chain to the last.
;;;
;;; 4. TRUNCATED TOOL CALLS. A response with finish reason :LENGTH and tool
;;;    calls: generation is sequential, so only the LAST call can be cut;
;;;    RESPONSE-TRUNCATED-CALLS returns it (a list of one part), and NIL for
;;;    any other finish reason. The loop then:
;;;    a. When settings extra :LENGTH-RETRY-MAX-TOKENS is an integer and this
;;;       step has not been retried for it yet: drop the response (as a
;;;       failed stream is dropped: %DISCARD-IN-FLIGHT; nothing was invoked),
;;;       emit :RETRY with reason :OUTPUT-TRUNCATED, and generate the step
;;;       again with the LLM settings' max tokens raised to that value (a
;;;       copy of the run's LLM-SETTINGS; the run's own settings are not
;;;       changed for later steps).
;;;    b. Otherwise: SIGNAL AGENT-OUTPUT-TRUNCATED (an AGENT-CONDITION, not
;;;       an error) with restart USE-VALUE (a replacement result text). The
;;;       truncated call is not invoked: its invocation gets status :ERROR,
;;;       error-p T and the result (FORMAT NIL +TRUNCATED-TOOL-CALL-FEEDBACK+
;;;       name max-tokens-or-"the") — or the USE-VALUE text. The other calls
;;;       of the response run as usual, then the loop continues, so the model
;;;       can retry with smaller arguments. Event (:truncated (:step S
;;;       :calls (ID …) :max-tokens N)).
;;;
;;; 5. MANUAL COMPACTION. COMPACT-AGENT-RUN run &key callback error-callback
;;;    instructions: on an idle run (finished, not :approval / :deferred),
;;;    compact its turns now with the run's :COMPACTION memory (forced: no
;;;    threshold), on the loop, off-loop summarising as %MAYBE-COMPACT does.
;;;    INSTRUCTIONS (a string) are appended to the memory's prompt for this
;;;    call only (a SUMMARY-MEMORY's prompt, LF LF, then the text). CALLBACK
;;;    gets T when something was compacted, else NIL. :COMPACTED with reason
;;;    :MANUAL. No :COMPACTION memory → AGENT-ERROR "no compaction memory".
;;;    A busy run → AGENT-ERROR "run is busy". The next CONTINUE-AI-AGENT-ASYNC
;;;    starts from the compacted turns. Instructions are forgotten as for any
;;;    compaction (%INSTRUCTIONS-COMPACTED).
;;;
;;; Mid-stream failures need no new rule: a generate step that fails is
;;; dropped and retried; tool results of earlier steps are already in
;;; history and are never re-run (tests pin it).

(defparameter +continuation-prompt+
  "Your previous reply was cut off at the output token limit. Continue exactly
where it stopped, without repeating anything and without an introduction."
  "User turn the loop adds to continue a reply that ended at the output limit.")

(defparameter +truncated-tool-call-feedback+
  "The call to ~a was cut off at the output limit (~a max tokens), so it was not
run: its arguments are incomplete. Retry with smaller arguments, for example
by splitting the content over several calls."
  "Tool result for a truncated call. FORMAT args: tool name, max tokens.")

(define-condition agent-output-truncated (agent-condition)
  ((invocation :initarg :invocation :reader agent-output-truncated-invocation
               :initform nil)
   (max-tokens :initarg :max-tokens :reader agent-output-truncated-max-tokens
               :initform nil))
  (:report (lambda (c s)
             (format s "model output truncated at the output limit~@[ (~a tokens)~]~@[: ~a~]"
                     (agent-output-truncated-max-tokens c)
                     (agent-error-message c)))))


;;; --- pure helpers ------------------------------------------------------------

(defun response-truncated-calls (response)
  "Rule 4: the tool call a :LENGTH RESPONSE cut off (the last one), as a list of
one part; NIL for any other finish reason or a response without calls."
  (when (eq :length (llm-response-finish-reason response))
    (let ((calls (remove-if-not #'llm-tool-call-part-p (llm-response-parts response))))
      (and calls (last calls)))))

(defun continuation-turn (&optional (prompt +continuation-prompt+))
  "Rule 3: the :USER turn that asks the model to continue a cut-off reply."
  (check-type prompt string)
  (make-llm-turn :role :user :parts (list (make-llm-text-part :text prompt))))

(defun continuation-turn-p (turn &optional (prompt +continuation-prompt+))
  "T when TURN is a CONTINUATION-TURN for PROMPT (tested on content)."
  (and (llm-turn-p turn)
       (eq :user (llm-turn-role turn))
       (let ((parts (llm-turn-parts turn)))
         (and parts
              (null (cdr parts))
              (llm-text-part-p (car parts))
              (stringp (llm-text-part-text (car parts)))
              (string= prompt (llm-text-part-text (car parts)))
              t))))

(defun %retry-payload (reason condition attempt delay &optional policy)
  "Rule 2: the :RETRY event payload. Every key is present, NIL when unknown."
  (let ((http (and (typep condition 'llm-http-error) condition)))
    (list :reason reason
          :attempt attempt
          :delay delay
          :condition condition
          :max-attempts (and (typep policy 'retry-policy)
                             (retry-policy-max-attempts policy))
          :status (and http (llm-http-error-status http))
          :retry-after (and http (llm-http-error-retry-after http)))))

;;; --- rule 1: a retry backoff is a wait ---------------------------------------
;;; Parked in AGENT-RUN-WAITER, as a run waiting for its subagents is:
;;; CANCEL-AGENT-RUN → %CANCEL-SUBAGENTS already calls the waiter of a canceled
;;; run, from any thread. The waiter only hops to the loop; there the timer and
;;; the wake both test DONE first, so exactly one of them acts.

(defun %backoff (run delay resume callback)
  "Wait DELAY seconds on the loop, then call RESUME unless the run was
canceled meanwhile. A cancel stops the timer and finishes the run :CANCELED
on the next loop turn (%CHECK-CANCELED: CONTINUE resumes at once)."
  (multiple-value-bind (eb el) (%event-context)
    (let ((done nil) (timer nil) (waiter nil))
      (labels ((settle ()
                 (setf done t)
                 (unless (%check-canceled run callback)
                   (funcall resume)))
               (wake-on-loop ()
                 (unless done
                   (if (%canceled-p run)
                       (progn
                         (when timer (event:cancel eb timer))
                         (settle))
                       ;; Some other wake (a post to the inbox): keep waiting.
                       (setf (agent-run-waiter run) waiter))))
               (fire ()
                 (unless done
                   (when (eq (agent-run-waiter run) waiter)
                     (setf (agent-run-waiter run) nil))
                   (settle))))
        (setf waiter (lambda () (event:wake-call eb el #'wake-on-loop))
              timer (event:sleep* eb el delay :callback #'fire)
              (agent-run-waiter run) waiter)
        timer))))

;;; --- rule 3: output continuation ---------------------------------------------

(defun %max-continuations (run)
  (let ((n (%extra run :max-continuations 0)))
    (if (typep n '(integer 0 *)) n 0)))

(defun %track-continuation (run response)
  "After RESPONSE is committed: extend or end the continuation chain."
  (let ((continuing (shiftf (%agent-run-continuing-p run) nil)))
    (if (and continuing (not (agent-run-wrap-up-p run)))
        (setf (%agent-run-chain-text run)
              (concatenate 'string
                           (or (%agent-run-chain-text run) "")
                           (or (llm-response-text response) ""))
              (%agent-run-chain-response run) response)
        (setf (%agent-run-chain-text run) nil
              (%agent-run-chain-response run) nil)))
  (unless (eq :length (llm-response-finish-reason response))
    (setf (%agent-run-continuations run) 0)))

(defun %continue-output-p (run reason)
  (and (eq reason :length)
       (< (%agent-run-continuations run) (%max-continuations run))))

(defun %continue-output (run response callback error-callback)
  "Rule 3: keep the cut-off reply, ask for the rest, take one more step."
  (let ((k (incf (%agent-run-continuations run))))
    (unless (eq response (%agent-run-chain-response run))
      ;; First link of a chain.
      (setf (%agent-run-chain-text run) (or (llm-response-text response) "")
            (%agent-run-chain-response run) response))
    (setf (%agent-run-continuing-p run) t
          (agent-run-turns run)
          (append (agent-run-turns run)
                  (list (continuation-turn
                         (%extra run :continuation-prompt +continuation-prompt+)))))
    (%emit run :continuation (list :step (agent-run-step run) :count k
                                   :max (%max-continuations run)))
    (%tick-run run callback error-callback)))

;;; --- rule 4: truncated tool calls --------------------------------------------

(defun %length-retry-step-p (run)
  "T while this step is regenerated with raised max tokens (rule 4a)."
  (and (not (agent-run-wrap-up-p run))
       (eql (%agent-run-length-retry-step run) (agent-run-step run))))

(defun %raised-llm-settings (llm max-tokens)
  "A fresh LLM-SETTINGS like LLM with MAX-TOKENS; LLM itself is not changed."
  (let ((s (coerce-settings llm)))
    (if (null s)
        (make-llm-settings :max-tokens max-tokens)
        (make-llm-settings :temperature (llm-settings-temperature s)
                           :max-tokens max-tokens
                           :stop (llm-settings-stop s)
                           :top-p (llm-settings-top-p s)
                           :response-format (llm-settings-response-format s)
                           :output (llm-settings-output s)
                           :output-repair (llm-settings-output-repair s)
                           :extra (llm-settings-extra s)))))

(defun %step-llm-settings (run)
  "LLM settings for this step's generate: the run's own, or in a length retry
a copy with max tokens raised to settings extra :LENGTH-RETRY-MAX-TOKENS."
  (let ((llm (agent-settings-llm (agent-run-settings run)))
        (raise (%extra run :length-retry-max-tokens)))
    (if (and (%length-retry-step-p run) (integerp raise))
        (%raised-llm-settings llm raise)
        llm)))

(defun %step-max-tokens (run)
  "The output limit this step's generate ran with, or NIL when unknown."
  (let ((s (coerce-settings (%step-llm-settings run))))
    (and s (llm-settings-max-tokens s))))

(defun %length-retry-p (run response)
  "Rule 4a applies to RESPONSE: a cut-off tool call, a raise configured, and
this step not yet retried for it."
  (and (not (agent-run-wrap-up-p run))
       (integerp (%extra run :length-retry-max-tokens))
       (not (%length-retry-step-p run))
       (response-truncated-calls response)
       t))

(defun %retry-truncated (run callback error-callback)
  "Rule 4a: drop the cut-off response (nothing was committed or invoked) and
generate the same step again with raised max tokens."
  (%discard-in-flight run)
  (setf (%agent-run-length-retry-step run) (agent-run-step run))
  (let ((raise (%extra run :length-retry-max-tokens)))
    (%emit run :retry
           (%retry-payload :output-truncated
                           (make-condition 'agent-output-truncated
                                           :run run
                                           :max-tokens raise
                                           :message "tool call cut off at the output limit")
                           (agent-run-generate-attempt run) 0)))
  (%do-generate run callback error-callback))

(defun %truncated-invocation (run call)
  "Rule 4b: CALL was cut off, so it is not classified or invoked. → an :ERROR
invocation whose result tells the model why (or a handler's USE-VALUE text).
%RECORD-TERMINAL-INVOCATIONS gives it its tool turn."
  (let* ((max (%step-max-tokens run))
         (name (llm-tool-call-part-name call))
         (inv (make-agent-invocation
               :id (or (llm-tool-call-part-id call)
                       (format nil "call-~a" (random (expt 36 8))))
               :name name
               :arguments (or (llm-tool-call-part-arguments call) "")
               :source (%find-source run name)))
         (text (restart-case
                   (progn
                     (signal 'agent-output-truncated
                             :run run :invocation inv :max-tokens max
                             :message (format nil "tool call ~a" name))
                     (format nil +truncated-tool-call-feedback+ name (or max "the")))
                 (use-value (result)
                   :report "Use a supplied tool result string for the truncated call"
                   :interactive (lambda ()
                                  (format *query-io* "Tool result: ")
                                  (force-output *query-io*)
                                  (list (read-line *query-io*)))
                   (if (stringp result) result (princ-to-string result))))))
    (setf (agent-invocation-status inv) :error
          (agent-invocation-error-p inv) t
          (agent-invocation-result inv) text)
    (%emit-invocation run inv)
    (%emit run :truncated (list :step (agent-run-step run)
                                :calls (list (agent-invocation-id inv))
                                :max-tokens max))
    inv))

;;; --- rule 5: manual compaction -----------------------------------------------

(defun %prompt-accessor (memory)
  "The summary prompt reader applicable to MEMORY, or NIL. Reached at run time
only: its methods live in conversation-protocol/summary, which the core system
does not depend on."
  (let ((name 'conversation:summary-memory-prompt))
    (and (fboundp name)
         (fboundp (list 'setf name))
         (compute-applicable-methods (fdefinition name) (list memory))
         name)))

(defun %compact-turns-with (memory turns backend instructions)
  "CONVERSATION:COMPACT-TURNS, with INSTRUCTIONS appended to MEMORY's summary
prompt for this call only (restored on any exit)."
  (let ((accessor (and instructions (%prompt-accessor memory))))
    (if (null accessor)
        (conversation:compact-turns memory turns :backend backend)
        (let ((setter (fdefinition (list 'setf accessor)))
              (old (funcall accessor memory)))
          (unwind-protect
               (progn
                 (funcall setter
                          (format nil "~@[~a~]~c~c~a" old #\Newline #\Newline instructions)
                          memory)
                 (conversation:compact-turns memory turns :backend backend))
            (funcall setter old memory))))))

(defgeneric compact-agent-run (run &key callback error-callback instructions)
  (:documentation "Compact an idle RUN's turns now (rule 5 in src/resilience.lisp).")
  (:method ((handle agent-run-handle) &rest keys &key &allow-other-keys)
    (apply #'compact-agent-run (agent-run-handle-run handle) keys))
  (:method ((run agent-run) &key callback error-callback instructions)
    (check-type instructions (or null string))
    (%event-context)
    (let ((ok (or callback (lambda (v) (declare (ignore v)))))
          (err (or error-callback #'error))
          (reason (agent-run-finish-reason run)))
      ;; The checks are plain reads, as in CONTINUE-AI-AGENT-ASYNC: refuse at
      ;; once. A continue that lands before the compaction applies makes
      ;; %MAYBE-COMPACT drop the result (CALLBACK gets NIL).
      (cond
        ((null (%compactor run))
         (funcall err (make-condition 'agent-error :run run
                                                   :message "no compaction memory")))
        ((or (null reason) (not (%terminal-finish-p reason)))
         (funcall err (make-condition 'agent-error :run run :message "run is busy")))
        (t
         (%call-on-loop
          (lambda ()
            (if (null (agent-run-finish-reason run)) ; continued meanwhile
                (funcall err (make-condition 'agent-error :run run
                                                          :message "run is busy"))
                (%maybe-compact
                 run (lambda (did) (funcall ok (and did t))) err
                 :force t :reason :manual :instructions instructions
                 :key (%segment-key run (format nil "compact/manual/~d"
                                                (incf (%agent-run-manual-compactions run))))))))))
      (agent-run-handle run))))
