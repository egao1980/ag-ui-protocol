(in-package #:ag-ui-protocol)

;;; Wire type strings match @ag-ui/core EventType (wave-1 subset).
;;; Reasoning / activity / subagent families are non-goals.

(defclass ag-ui-event ()
  ((type :initarg :type :accessor ag-ui-event-type)
   (timestamp :initarg :timestamp :initform nil :accessor ag-ui-event-timestamp)
   (raw-event :initarg :raw-event :initform nil :accessor ag-ui-event-raw-event)
   (metadata :initarg :metadata :initform nil :accessor ag-ui-event-metadata)))

(defclass run-started-event (ag-ui-event)
  ((thread-id :initarg :thread-id :accessor run-started-thread-id)
   (run-id :initarg :run-id :accessor run-started-run-id)
   (parent-run-id :initarg :parent-run-id :initform nil
                  :accessor run-started-parent-run-id)
   (input :initarg :input :initform nil :accessor run-started-input)))

(defclass run-finished-event (ag-ui-event)
  ((thread-id :initarg :thread-id :accessor run-finished-thread-id)
   (run-id :initarg :run-id :accessor run-finished-run-id)
   (result :initarg :result :initform nil :accessor run-finished-result)))

(defclass run-error-event (ag-ui-event)
  ((message :initarg :message :accessor run-error-message)
   (code :initarg :code :initform nil :accessor run-error-code)))

(defclass step-started-event (ag-ui-event)
  ((step-name :initarg :step-name :accessor step-event-name)))

(defclass step-finished-event (ag-ui-event)
  ((step-name :initarg :step-name :accessor step-event-name)))

(defclass text-message-start-event (ag-ui-event)
  ((message-id :initarg :message-id :accessor text-message-id)
   (role :initarg :role :initform "assistant" :accessor text-message-role)))

(defclass text-message-content-event (ag-ui-event)
  ((message-id :initarg :message-id :accessor text-message-id)
   (delta :initarg :delta :accessor text-message-delta)))

(defclass text-message-end-event (ag-ui-event)
  ((message-id :initarg :message-id :accessor text-message-id)))

(defclass tool-call-start-event (ag-ui-event)
  ((tool-call-id :initarg :tool-call-id :accessor tool-call-id)
   (tool-call-name :initarg :tool-call-name :accessor tool-call-name)
   (parent-message-id :initarg :parent-message-id :initform nil
                      :accessor tool-call-parent-message-id)))

(defclass tool-call-args-event (ag-ui-event)
  ((tool-call-id :initarg :tool-call-id :accessor tool-call-id)
   (delta :initarg :delta :accessor tool-call-delta)))

(defclass tool-call-end-event (ag-ui-event)
  ((tool-call-id :initarg :tool-call-id :accessor tool-call-id)))

(defclass tool-call-result-event (ag-ui-event)
  ((message-id :initarg :message-id :accessor text-message-id)
   (tool-call-id :initarg :tool-call-id :accessor tool-call-id)
   (content :initarg :content :accessor tool-call-result-content)
   (role :initarg :role :initform "tool" :accessor tool-call-result-role)))

(defclass state-snapshot-event (ag-ui-event)
  ((snapshot :initarg :snapshot :accessor state-snapshot-value)))

(defclass state-delta-event (ag-ui-event)
  ((delta :initarg :delta :accessor state-delta-patch)))

(defclass messages-snapshot-event (ag-ui-event)
  ((messages :initarg :messages :initform nil :accessor messages-snapshot-messages)))

(defclass ag-ui-message ()
  ((id :initarg :id :accessor ag-ui-message-id)
   (role :initarg :role :accessor ag-ui-message-role)
   (content :initarg :content :initform nil :accessor ag-ui-message-content)
   (name :initarg :name :initform nil :accessor ag-ui-message-name)
   (tool-call-id :initarg :tool-call-id :initform nil
                 :accessor ag-ui-message-tool-call-id)
   (tool-calls :initarg :tool-calls :initform nil
               :accessor ag-ui-message-tool-calls)))

(defclass ag-ui-tool ()
  ((name :initarg :name :accessor ag-ui-tool-name)
   (description :initarg :description :initform "" :accessor ag-ui-tool-description)
   (parameters :initarg :parameters :initform nil :accessor ag-ui-tool-parameters)))

(defclass ag-ui-context ()
  ((description :initarg :description :accessor ag-ui-context-description)
   (value :initarg :value :accessor ag-ui-context-value)))

(defclass run-agent-input ()
  ((thread-id :initarg :thread-id :accessor run-agent-input-thread-id)
   (run-id :initarg :run-id :accessor run-agent-input-run-id)
   (parent-run-id :initarg :parent-run-id :initform nil
                  :accessor run-agent-input-parent-run-id)
   (state :initarg :state :initform nil :accessor run-agent-input-state)
   (messages :initarg :messages :initform nil :accessor run-agent-input-messages)
   (tools :initarg :tools :initform nil :accessor run-agent-input-tools)
   (context :initarg :context :initform nil :accessor run-agent-input-context)
   (forwarded-props :initarg :forwarded-props :initform nil
                    :accessor run-agent-input-forwarded-props)))

(defclass ag-ui-agent ()
  ((name :initarg :name :initform "agent" :accessor ag-ui-agent-name)
   (handler :initarg :handler :initform nil :accessor ag-ui-agent-handler)))

(defclass ag-ui-backend () ())

(defvar *ag-ui-backend* nil)

(defun make-ag-ui-message (&key id role content name tool-call-id tool-calls)
  (make-instance 'ag-ui-message
                 :id (or id (format nil "msg-~a" (random (expt 36 6))))
                 :role (or role "user")
                 :content content :name name
                 :tool-call-id tool-call-id :tool-calls tool-calls))

(defun make-ag-ui-tool (&key name (description "") parameters)
  (make-instance 'ag-ui-tool :name name :description description
                             :parameters parameters))

(defun make-ag-ui-context (&key description value)
  (make-instance 'ag-ui-context :description description :value value))

(defun make-run-agent-input (&key thread-id run-id parent-run-id state
                               messages tools context forwarded-props)
  (make-instance 'run-agent-input
                 :thread-id thread-id :run-id run-id
                 :parent-run-id parent-run-id :state state
                 :messages messages :tools tools :context context
                 :forwarded-props forwarded-props))

(defun make-ag-ui-agent (&key (name "agent") handler)
  (make-instance 'ag-ui-agent :name name :handler handler))

(defun make-run-started-event (&key thread-id run-id parent-run-id input timestamp)
  (make-instance 'run-started-event :type "RUN_STARTED"
                 :thread-id thread-id :run-id run-id
                 :parent-run-id parent-run-id :input input :timestamp timestamp))

(defun make-run-finished-event (&key thread-id run-id result timestamp)
  (make-instance 'run-finished-event :type "RUN_FINISHED"
                 :thread-id thread-id :run-id run-id
                 :result result :timestamp timestamp))

(defun make-run-error-event (&key message code timestamp)
  (make-instance 'run-error-event :type "RUN_ERROR"
                 :message message :code code :timestamp timestamp))

(defun make-step-started-event (&key step-name timestamp)
  (make-instance 'step-started-event :type "STEP_STARTED"
                 :step-name step-name :timestamp timestamp))

(defun make-step-finished-event (&key step-name timestamp)
  (make-instance 'step-finished-event :type "STEP_FINISHED"
                 :step-name step-name :timestamp timestamp))

(defun make-text-message-start-event (&key message-id (role "assistant") timestamp)
  (make-instance 'text-message-start-event :type "TEXT_MESSAGE_START"
                 :message-id message-id :role role :timestamp timestamp))

(defun make-text-message-content-event (&key message-id delta timestamp)
  (make-instance 'text-message-content-event :type "TEXT_MESSAGE_CONTENT"
                 :message-id message-id :delta delta :timestamp timestamp))

(defun make-text-message-end-event (&key message-id timestamp)
  (make-instance 'text-message-end-event :type "TEXT_MESSAGE_END"
                 :message-id message-id :timestamp timestamp))

(defun make-tool-call-start-event (&key tool-call-id tool-call-name
                                     parent-message-id timestamp)
  (make-instance 'tool-call-start-event :type "TOOL_CALL_START"
                 :tool-call-id tool-call-id :tool-call-name tool-call-name
                 :parent-message-id parent-message-id :timestamp timestamp))

(defun make-tool-call-args-event (&key tool-call-id delta timestamp)
  (make-instance 'tool-call-args-event :type "TOOL_CALL_ARGS"
                 :tool-call-id tool-call-id :delta delta :timestamp timestamp))

(defun make-tool-call-end-event (&key tool-call-id timestamp)
  (make-instance 'tool-call-end-event :type "TOOL_CALL_END"
                 :tool-call-id tool-call-id :timestamp timestamp))

(defun make-tool-call-result-event (&key message-id tool-call-id content
                                      (role "tool") timestamp)
  (make-instance 'tool-call-result-event :type "TOOL_CALL_RESULT"
                 :message-id message-id :tool-call-id tool-call-id
                 :content content :role role :timestamp timestamp))

(defun make-state-snapshot-event (&key snapshot timestamp)
  (make-instance 'state-snapshot-event :type "STATE_SNAPSHOT"
                 :snapshot snapshot :timestamp timestamp))

(defun make-state-delta-event (&key delta timestamp)
  (make-instance 'state-delta-event :type "STATE_DELTA"
                 :delta delta :timestamp timestamp))

(defun make-messages-snapshot-event (&key messages timestamp)
  (make-instance 'messages-snapshot-event :type "MESSAGES_SNAPSHOT"
                 :messages messages :timestamp timestamp))
