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

(defun %h3f-pending (name)
  (error "ai-agent-protocol 0.16.0: ~a is not implemented yet (H3f contract)" name))

(defun response-truncated-calls (response)
  (declare (ignore response))
  (%h3f-pending 'response-truncated-calls))

(defun continuation-turn (&optional (prompt +continuation-prompt+))
  (declare (ignore prompt))
  (%h3f-pending 'continuation-turn))

(defun continuation-turn-p (turn &optional (prompt +continuation-prompt+))
  (declare (ignore turn prompt))
  (%h3f-pending 'continuation-turn-p))

(defgeneric compact-agent-run (run &key callback error-callback instructions)
  (:documentation "Compact an idle RUN's turns now (rule 5 in src/resilience.lisp).")
  (:method (run &key callback error-callback instructions)
    (declare (ignore run callback error-callback instructions))
    (%h3f-pending 'compact-agent-run)))
