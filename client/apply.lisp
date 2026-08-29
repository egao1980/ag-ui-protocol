(in-package #:ag-ui-client)

;;; The reducer.
;;;
;;; Folds an event stream into the two things a consumer actually wants: the
;;; conversation as AG-UI messages, and the shared state document. Messages come
;;; out in the same shape RunAgentInput takes, so the next turn is just handing
;;; them back.

(defclass agent-state ()
  ((messages :initform '() :accessor %messages)
   (value :initarg :value :initform nil :accessor agent-state-value
          :documentation "The shared state document, per STATE_SNAPSHOT/DELTA.")
   (status :initform :idle :accessor agent-state-status
           :documentation ":idle, :running, :finished, :interrupted, or :error.")
   (error-message :initform nil :accessor agent-state-error-message)
   (active-steps :initform '() :accessor agent-state-active-steps)
   (interrupts :initform '() :accessor agent-state-interrupts
               :documentation "Interrupts left open by an interrupted run.")))

(defun make-agent-state (&key value)
  (make-instance 'agent-state :value value))

(defun agent-state-messages (state)
  "Conversation so far, oldest first. Feed straight back as RunAgentInput
   messages."
  (copy-list (%messages state)))

(defun message-text (message)
  (let ((content (ag-ui:event-field message 'ag-ui::content)))
    (if (stringp content) content "")))

(defun %find-message (state id)
  (find id (%messages state) :key #'ag-ui:ag-ui-message-id :test #'equal))

(defun %append-message (state message)
  (setf (%messages state) (append (%messages state) (list message)))
  message)

(defun %ensure-message (state id &key (role "assistant") activity-type)
  (or (%find-message state id)
      (%append-message state
                       (ag-ui:make-ag-ui-message
                        :id id :role role :content
                        (if (equal role "activity") nil "")
                        :activity-type activity-type))))

(defun %append-content (message delta)
  (setf (ag-ui:ag-ui-message-content message)
        (concatenate 'string (message-text message) (or delta ""))))

(defgeneric apply-event (state event)
  (:documentation "Fold EVENT into STATE, returning STATE.

   Unhandled event types are deliberately no-ops rather than errors: a stream
   may legally carry types this reducer has no opinion about, including ones
   from a newer producer."))

(defmethod apply-event ((state agent-state) (event ag-ui:ag-ui-event))
  state)

;;; Run lifecycle

(defmethod apply-event ((state agent-state) (event ag-ui:run-started-event))
  (setf (agent-state-status state) :running
        (agent-state-error-message state) nil
        (agent-state-interrupts state) '())
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:run-finished-event))
  (let ((open (ag-ui:open-interrupts event)))
    (setf (agent-state-interrupts state) open
          (agent-state-status state) (if open :interrupted :finished)))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:run-error-event))
  (setf (agent-state-status state) :error
        (agent-state-error-message state) (ag-ui:run-error-message event))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:step-started-event))
  (pushnew (ag-ui:step-event-name event) (agent-state-active-steps state)
           :test #'equal)
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:step-finished-event))
  (setf (agent-state-active-steps state)
        (remove (ag-ui:step-event-name event) (agent-state-active-steps state)
                :test #'equal))
  state)

;;; Text

(defmethod apply-event ((state agent-state) (event ag-ui:text-message-start-event))
  (%ensure-message state (ag-ui:text-message-id event)
                   :role (or (ag-ui:event-field event 'ag-ui::role) "assistant"))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:text-message-content-event))
  (%append-content (%ensure-message state (ag-ui:text-message-id event))
                   (ag-ui:text-message-delta event))
  state)

;;; Tool calls. A tool call belongs to an assistant message; when the producer
;;; names no parent we synthesise one, since a bare tool call has nowhere to live.

(defun %tool-call-table (id name)
  (ag-ui:json-object
   "id" id "type" "function"
   "function" (ag-ui:json-object "name" (or name "") "arguments" "")))

(defun %message-tool-calls (message)
  (coerce (or (ag-ui:event-field message 'ag-ui::tool-calls) #()) 'list))

(defun %set-tool-calls (message calls)
  (setf (ag-ui:ag-ui-message-tool-calls message) (coerce calls 'vector)))

(defun %find-tool-call (state id)
  (dolist (message (%messages state))
    (let ((hit (find-if (lambda (call) (equal id (gethash "id" call)))
                        (%message-tool-calls message))))
      (when hit (return (values hit message))))))

(defmethod apply-event ((state agent-state) (event ag-ui:tool-call-start-event))
  (let* ((parent-id (or (ag-ui:event-field event 'ag-ui::parent-message-id)
                        (format nil "asst-~a" (ag-ui:tool-call-id event))))
         (message (%ensure-message state parent-id :role "assistant")))
    (%set-tool-calls message
                     (append (%message-tool-calls message)
                             (list (%tool-call-table (ag-ui:tool-call-id event)
                                                     (ag-ui:tool-call-name event))))))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:tool-call-args-event))
  (let ((call (%find-tool-call state (ag-ui:tool-call-id event))))
    (when call
      (let ((fn (gethash "function" call)))
        (setf (gethash "arguments" fn)
              (concatenate 'string (or (gethash "arguments" fn) "")
                           (or (ag-ui:tool-call-delta event) ""))))))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:tool-call-result-event))
  (%append-message state
                   (ag-ui:make-ag-ui-message
                    :id (ag-ui:text-message-id event)
                    :role (or (ag-ui:event-field event 'ag-ui::role) "tool")
                    :tool-call-id (ag-ui:tool-call-id event)
                    :content (ag-ui:tool-call-result-content event)))
  state)

;;; Reasoning

(defmethod apply-event ((state agent-state)
                        (event ag-ui:reasoning-message-start-event))
  (%ensure-message state (ag-ui:text-message-id event) :role "reasoning")
  state)

(defmethod apply-event ((state agent-state)
                        (event ag-ui:reasoning-message-content-event))
  (%append-content (%ensure-message state (ag-ui:text-message-id event)
                                    :role "reasoning")
                   (ag-ui:text-message-delta event))
  state)

(defmethod apply-event ((state agent-state)
                        (event ag-ui:reasoning-encrypted-value-event))
  ;; Attaches to a message or a tool call; the client stores it opaquely and
  ;; echoes it back on the next turn.
  (let ((entity (ag-ui:reasoning-encrypted-entity-id event))
        (blob (ag-ui:reasoning-encrypted-value event)))
    (if (equal (ag-ui:reasoning-encrypted-subtype event) "tool-call")
        (let ((call (%find-tool-call state entity)))
          (when call (setf (gethash "encryptedValue" call) blob)))
        (let ((message (%find-message state entity)))
          (when message
            (setf (ag-ui:ag-ui-message-encrypted-value message) blob)))))
  state)

;;; State

(defmethod apply-event ((state agent-state) (event ag-ui:state-snapshot-event))
  ;; A snapshot replaces rather than merges — it is the resynchronisation point.
  (setf (agent-state-value state) (ag-ui:state-snapshot-value event))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:state-delta-event))
  (setf (agent-state-value state)
        (patch:apply-patch (agent-state-value state)
                           (coerce (ag-ui:state-delta-patch event) 'list)))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:messages-snapshot-event))
  ;; Activity and reasoning messages are all-or-nothing: a snapshot that carries
  ;; none says nothing about those roles, so the client keeps the ones it has.
  ;; Both are client-side by default, so a backend that tracks neither can omit
  ;; them without the client losing anything.
  (let* ((incoming (coerce (or (ag-ui:messages-snapshot-messages event) #()) 'list))
         (roles (remove-duplicates (mapcar #'ag-ui:ag-ui-message-role incoming)
                                   :test #'equal))
         (kept (remove-if-not
                (lambda (message)
                  (let ((role (ag-ui:ag-ui-message-role message)))
                    (and (member role '("activity" "reasoning") :test #'equal)
                         (not (member role roles :test #'equal)))))
                (%messages state))))
    (setf (%messages state) (append incoming kept)))
  state)

;;; Activity

(defun %activity-replaces-p (event)
  "The snapshot's `replace` flag. Absent defaults to true, so this cannot go
   through EVENT-FIELD: an omitted flag and an explicit false both read as NIL
   there but mean opposite things."
  (if (slot-boundp event 'cl:replace)
      (and (slot-value event 'cl:replace) t)
      t))

(defmethod apply-event ((state agent-state) (event ag-ui:activity-snapshot-event))
  (let* ((id (ag-ui:text-message-id event))
         (existing (%find-message state id)))
    ;; replace=false means "only if absent", for a producer resending a baseline
    ;; it does not want to clobber a live activity with.
    (unless (and existing (not (%activity-replaces-p event)))
      (let ((message (%ensure-message state id :role "activity"
                                              :activity-type (ag-ui:activity-type event))))
        (setf (ag-ui:ag-ui-message-activity-type message) (ag-ui:activity-type event)
              (ag-ui:ag-ui-message-content message) (ag-ui:activity-content event)))))
  state)

(defmethod apply-event ((state agent-state) (event ag-ui:activity-delta-event))
  (let ((message (%find-message state (ag-ui:text-message-id event))))
    (when message
      (setf (ag-ui:ag-ui-message-content message)
            (patch:apply-patch (ag-ui:ag-ui-message-content message)
                               (coerce (ag-ui:activity-patch event) 'list)))))
  state)

;;; Entry points

(defun apply-events (state events)
  "Fold EVENTS into STATE in order, returning STATE."
  (map nil (lambda (event) (apply-event state event)) events)
  state)

(defun reduce-events (events &key value verify (expand t))
  "Fold EVENTS into a fresh AGENT-STATE.

   EXPAND rewrites *_CHUNK events into their triads first, so a producer's
   choice of spelling does not reach the reducer. VERIFY additionally checks
   ordering, which is worth doing on anything arriving over a wire."
  (let ((events (if expand (ag-ui:expand-ag-ui-chunks (coerce events 'list)) events)))
    (when verify (verify-events events :complete nil))
    (apply-events (make-agent-state :value value) events)))
