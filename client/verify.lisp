(in-package #:ag-ui-client)

;;; Ordering verification.
;;;
;;; A malformed stream is far cheaper to diagnose at the boundary than three
;;; layers down in a reducer that quietly built the wrong transcript. These are
;;; the invariants the protocol guarantees, checked once, on arrival.

(define-condition ag-ui-verify-error (ag-ui:ag-ui-error)
  ((event :initarg :event :initform nil :reader ag-ui-verify-error-event))
  (:documentation "An event arrived out of order or unmatched."))

(defclass stream-verifier ()
  ((started :initform nil :accessor verifier-started-p)
   (terminated :initform nil :accessor verifier-terminated-p)
   (text-id :initform nil :accessor verifier-text-id)
   (tool-id :initform nil :accessor verifier-tool-id)
   (reasoning-id :initform nil :accessor verifier-reasoning-id)
   (reasoning-open :initform nil :accessor verifier-reasoning-open)
   (steps :initform '() :accessor verifier-steps)
   (subagents :initform '() :accessor verifier-subagents))
  (:documentation "Ordering state for one run. One verifier per stream."))

(defun make-stream-verifier ()
  (make-instance 'stream-verifier))

(defun %verify-fail (event control &rest args)
  (error 'ag-ui-verify-error :event event :message (apply #'format nil control args)))

(defun %type-of-event (event)
  (ag-ui:ag-ui-event-type event))

(defgeneric verify-event (verifier event)
  (:documentation "Check EVENT against VERIFIER's state, updating it.

   Signals AG-UI-VERIFY-ERROR when the stream violates ordering. Returns EVENT
   so this can sit inline in a pipeline."))

(defmethod verify-event :before ((v stream-verifier) event)
  (let ((type (%type-of-event event)))
    (when (verifier-terminated-p v)
      (%verify-fail event "~a arrived after the run already ended" type))
    (unless (or (verifier-started-p v) (equal type "RUN_STARTED"))
      (%verify-fail event "first event must be RUN_STARTED, got ~a" type))))

(defmethod verify-event ((v stream-verifier) (event ag-ui:ag-ui-event))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:run-started-event))
  (when (verifier-started-p v)
    (%verify-fail event "RUN_STARTED sent twice in one stream"))
  (setf (verifier-started-p v) t)
  event)

(defun %check-nothing-open (v event type)
  (when (verifier-text-id v)
    (%verify-fail event "~a while text message ~a is still open"
                  type (verifier-text-id v)))
  (when (verifier-tool-id v)
    (%verify-fail event "~a while tool call ~a is still open"
                  type (verifier-tool-id v)))
  (when (verifier-reasoning-id v)
    (%verify-fail event "~a while reasoning message ~a is still open"
                  type (verifier-reasoning-id v)))
  (when (verifier-steps v)
    (%verify-fail event "~a with step~p still open: ~{~a~^ ~}"
                  type (length (verifier-steps v)) (reverse (verifier-steps v)))))

(defmethod verify-event ((v stream-verifier) (event ag-ui:run-finished-event))
  (%check-nothing-open v event "RUN_FINISHED")
  (setf (verifier-terminated-p v) t)
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:run-error-event))
  ;; RUN_ERROR may cut a run short, so open sequences are expected here.
  (setf (verifier-terminated-p v) t)
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:step-started-event))
  (let ((name (ag-ui:step-event-name event)))
    (when (member name (verifier-steps v) :test #'equal)
      (%verify-fail event "step ~a started twice" name))
    (push name (verifier-steps v)))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:step-finished-event))
  (let ((name (ag-ui:step-event-name event)))
    (unless (member name (verifier-steps v) :test #'equal)
      (%verify-fail event "STEP_FINISHED for ~a which was never started" name))
    (setf (verifier-steps v) (remove name (verifier-steps v) :test #'equal)))
  event)

;;; Text messages

(defmethod verify-event ((v stream-verifier) (event ag-ui:text-message-start-event))
  (when (verifier-text-id v)
    (%verify-fail event "TEXT_MESSAGE_START while ~a is still open"
                  (verifier-text-id v)))
  (when (verifier-tool-id v)
    (%verify-fail event "TEXT_MESSAGE_START while tool call ~a is still open"
                  (verifier-tool-id v)))
  (setf (verifier-text-id v) (ag-ui:text-message-id event))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:text-message-content-event))
  (let ((id (ag-ui:text-message-id event)))
    (unless (equal id (verifier-text-id v))
      (%verify-fail event "TEXT_MESSAGE_CONTENT for ~a with no open message" id)))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:text-message-end-event))
  (let ((id (ag-ui:text-message-id event)))
    (unless (equal id (verifier-text-id v))
      (%verify-fail event "TEXT_MESSAGE_END for ~a with no open message" id))
    (setf (verifier-text-id v) nil))
  event)

;;; Tool calls

(defmethod verify-event ((v stream-verifier) (event ag-ui:tool-call-start-event))
  (when (verifier-tool-id v)
    (%verify-fail event "TOOL_CALL_START while ~a is still open"
                  (verifier-tool-id v)))
  (setf (verifier-tool-id v) (ag-ui:tool-call-id event))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:tool-call-args-event))
  (let ((id (ag-ui:tool-call-id event)))
    (unless (equal id (verifier-tool-id v))
      (%verify-fail event "TOOL_CALL_ARGS for ~a with no open tool call" id)))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:tool-call-end-event))
  (let ((id (ag-ui:tool-call-id event)))
    (unless (equal id (verifier-tool-id v))
      (%verify-fail event "TOOL_CALL_END for ~a with no open tool call" id))
    (setf (verifier-tool-id v) nil))
  event)

;;; Reasoning

(defmethod verify-event ((v stream-verifier) (event ag-ui:reasoning-start-event))
  (setf (verifier-reasoning-open v) (ag-ui:text-message-id event))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:reasoning-end-event))
  (let ((id (ag-ui:text-message-id event)))
    (unless (equal id (verifier-reasoning-open v))
      (%verify-fail event "REASONING_END for ~a which was never started" id))
    (setf (verifier-reasoning-open v) nil))
  event)

(defmethod verify-event ((v stream-verifier)
                         (event ag-ui:reasoning-message-start-event))
  (when (verifier-reasoning-id v)
    (%verify-fail event "REASONING_MESSAGE_START while ~a is still open"
                  (verifier-reasoning-id v)))
  (setf (verifier-reasoning-id v) (ag-ui:text-message-id event))
  event)

(defmethod verify-event ((v stream-verifier)
                         (event ag-ui:reasoning-message-content-event))
  (let ((id (ag-ui:text-message-id event)))
    (unless (equal id (verifier-reasoning-id v))
      (%verify-fail event "REASONING_MESSAGE_CONTENT for ~a with no open message" id)))
  event)

(defmethod verify-event ((v stream-verifier)
                         (event ag-ui:reasoning-message-end-event))
  (let ((id (ag-ui:text-message-id event)))
    (unless (equal id (verifier-reasoning-id v))
      (%verify-fail event "REASONING_MESSAGE_END for ~a with no open message" id))
    (setf (verifier-reasoning-id v) nil))
  event)

;;; Subagents

(defmethod verify-event ((v stream-verifier) (event ag-ui:subagent-started-event))
  (let ((id (ag-ui:ag-ui-event-subagent-run-id event)))
    (unless id
      (%verify-fail event "SUBAGENT_STARTED without a subagentRunId"))
    (when (member id (verifier-subagents v) :test #'equal)
      (%verify-fail event "subagent ~a started twice" id))
    (push id (verifier-subagents v)))
  event)

(defun %close-subagent (v event)
  (let ((id (ag-ui:ag-ui-event-subagent-run-id event)))
    (unless id
      (%verify-fail event "~a without a subagentRunId" (%type-of-event event)))
    (unless (member id (verifier-subagents v) :test #'equal)
      (%verify-fail event "~a for subagent ~a which was never started"
                    (%type-of-event event) id))
    (setf (verifier-subagents v) (remove id (verifier-subagents v) :test #'equal)))
  event)

(defmethod verify-event ((v stream-verifier) (event ag-ui:subagent-finished-event))
  (%close-subagent v event))

(defmethod verify-event ((v stream-verifier) (event ag-ui:subagent-error-event))
  (%close-subagent v event))

(defun finish-verify (verifier)
  "Check that the stream ended in a legal state. Returns T."
  (unless (verifier-started-p verifier)
    (error 'ag-ui-verify-error :message "stream ended without any RUN_STARTED"))
  (unless (verifier-terminated-p verifier)
    (error 'ag-ui-verify-error
           :message "stream ended without RUN_FINISHED or RUN_ERROR"))
  t)

(defun verify-events (events &key (complete t))
  "Verify a whole sequence. COMPLETE also requires the run to have terminated.
   Returns EVENTS so this can wrap a stream in place."
  (let ((verifier (make-stream-verifier)))
    (map nil (lambda (event) (verify-event verifier event)) events)
    (when complete (finish-verify verifier))
    events))
