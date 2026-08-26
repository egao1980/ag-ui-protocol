(in-package #:ag-ui-protocol)

;;; Interchange models = schema-protocol (API payloads, not persistence).
;;; Wire keys = camelCase to match @ag-ui/core. Event dispatch = :tag type.

(defun %make (class &rest keys)
  "MAKE-INSTANCE dropping NIL optionals so DUMP omits them."
  (apply #'make-instance class
         (loop for (k v) on keys by #'cddr
               when v
                 append (list k v))))

(stack-schema:defschema ag-ui-message ()
  (id string :accessor ag-ui-message-id)
  (role string :accessor ag-ui-message-role)
  (content t :optional t :accessor ag-ui-message-content)
  (name string :optional t :accessor ag-ui-message-name)
  (tool-call-id string :optional t :accessor ag-ui-message-tool-call-id)
  (tool-calls (vector hash-table) :optional t :accessor ag-ui-message-tool-calls)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema ag-ui-tool ()
  "Tool descriptor. PARAMETERS is a JSON Schema object (draft-07)."
  (name string :accessor ag-ui-tool-name)
  (description string :default "" :accessor ag-ui-tool-description)
  (parameters hash-table :optional t :accessor ag-ui-tool-parameters)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema ag-ui-context ()
  (description string :accessor ag-ui-context-description)
  (value string :accessor ag-ui-context-value)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema run-agent-input ()
  (thread-id string :accessor run-agent-input-thread-id)
  (run-id string :accessor run-agent-input-run-id)
  (parent-run-id string :optional t :accessor run-agent-input-parent-run-id)
  (state t :optional t :accessor run-agent-input-state)
  (messages (vector ag-ui-message) :default #() :accessor run-agent-input-messages)
  (tools (vector ag-ui-tool) :default #() :accessor run-agent-input-tools)
  (context (vector ag-ui-context) :default #() :accessor run-agent-input-context)
  (forwarded-props hash-table :optional t :accessor run-agent-input-forwarded-props)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema ag-ui-event ()
  (event-type string :key "type" :accessor ag-ui-event-type)
  (timestamp number :optional t :accessor ag-ui-event-timestamp)
  (raw-event t :optional t :accessor ag-ui-event-raw-event)
  (metadata hash-table :optional t :accessor ag-ui-event-metadata)
  (:tag event-type)
  (:key-style :camel)
  (:extra :allow))

;;; Subclasses must repeat :key-style — schema-protocol does not inherit it
;;; (initform :downcase), so dump/parse would emit thread-id instead of threadId.

(stack-schema:defschema run-started-event (ag-ui-event)
  (event-type (eql "RUN_STARTED") :default "RUN_STARTED" :key "type")
  (thread-id string :accessor run-started-thread-id)
  (run-id string :accessor run-started-run-id)
  (parent-run-id string :optional t :accessor run-started-parent-run-id)
  (input run-agent-input :optional t :accessor run-started-input)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema run-finished-event (ag-ui-event)
  (event-type (eql "RUN_FINISHED") :default "RUN_FINISHED" :key "type")
  (thread-id string :accessor run-finished-thread-id)
  (run-id string :accessor run-finished-run-id)
  (result t :optional t :accessor run-finished-result)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema run-error-event (ag-ui-event)
  (event-type (eql "RUN_ERROR") :default "RUN_ERROR" :key "type")
  (message string :accessor run-error-message)
  (code string :optional t :accessor run-error-code)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema step-started-event (ag-ui-event)
  (event-type (eql "STEP_STARTED") :default "STEP_STARTED" :key "type")
  (step-name string :accessor step-event-name)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema step-finished-event (ag-ui-event)
  (event-type (eql "STEP_FINISHED") :default "STEP_FINISHED" :key "type")
  (step-name string :accessor step-event-name)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema text-message-start-event (ag-ui-event)
  (event-type (eql "TEXT_MESSAGE_START") :default "TEXT_MESSAGE_START" :key "type")
  (message-id string :accessor text-message-id)
  (role string :default "assistant" :accessor text-message-role)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema text-message-content-event (ag-ui-event)
  (event-type (eql "TEXT_MESSAGE_CONTENT") :default "TEXT_MESSAGE_CONTENT" :key "type")
  (message-id string :accessor text-message-id)
  (delta string :accessor text-message-delta)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema text-message-end-event (ag-ui-event)
  (event-type (eql "TEXT_MESSAGE_END") :default "TEXT_MESSAGE_END" :key "type")
  (message-id string :accessor text-message-id)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema tool-call-start-event (ag-ui-event)
  (event-type (eql "TOOL_CALL_START") :default "TOOL_CALL_START" :key "type")
  (tool-call-id string :accessor tool-call-id)
  (tool-call-name string :accessor tool-call-name)
  (parent-message-id string :optional t :accessor tool-call-parent-message-id)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema tool-call-args-event (ag-ui-event)
  (event-type (eql "TOOL_CALL_ARGS") :default "TOOL_CALL_ARGS" :key "type")
  (tool-call-id string :accessor tool-call-id)
  (delta string :accessor tool-call-delta)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema tool-call-end-event (ag-ui-event)
  (event-type (eql "TOOL_CALL_END") :default "TOOL_CALL_END" :key "type")
  (tool-call-id string :accessor tool-call-id)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema tool-call-result-event (ag-ui-event)
  (event-type (eql "TOOL_CALL_RESULT") :default "TOOL_CALL_RESULT" :key "type")
  (message-id string :accessor text-message-id)
  (tool-call-id string :accessor tool-call-id)
  (content string :accessor tool-call-result-content)
  (role string :default "tool" :optional t :accessor tool-call-result-role)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema state-snapshot-event (ag-ui-event)
  (event-type (eql "STATE_SNAPSHOT") :default "STATE_SNAPSHOT" :key "type")
  (snapshot t :accessor state-snapshot-value)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema state-delta-event (ag-ui-event)
  (event-type (eql "STATE_DELTA") :default "STATE_DELTA" :key "type")
  (delta (vector hash-table) :accessor state-delta-patch)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema messages-snapshot-event (ag-ui-event)
  (event-type (eql "MESSAGES_SNAPSHOT") :default "MESSAGES_SNAPSHOT" :key "type")
  (messages (vector ag-ui-message) :default #() :accessor messages-snapshot-messages)
  (:key-style :camel)
  (:extra :allow))

(defclass ag-ui-agent ()
  ((name :initarg :name :initform "agent" :accessor ag-ui-agent-name)
   (handler :initarg :handler :initform nil :accessor ag-ui-agent-handler)))

(defclass ag-ui-backend () ())

(defvar *ag-ui-backend* nil)

(defun make-ag-ui-message (&key id role content name tool-call-id tool-calls)
  (%make 'ag-ui-message
         :id (or id (format nil "msg-~a" (random (expt 36 6))))
         :role (or role "user")
         :content content :name name
         :tool-call-id tool-call-id :tool-calls tool-calls))

(defun make-ag-ui-tool (&key name (description "") parameters)
  (%make 'ag-ui-tool :name name :description description :parameters parameters))

(defun make-ag-ui-context (&key description value)
  (%make 'ag-ui-context :description description :value value))

(defun make-run-agent-input (&key thread-id run-id parent-run-id state
                               messages tools context forwarded-props)
  (%make 'run-agent-input
         :thread-id thread-id :run-id run-id
         :parent-run-id parent-run-id :state state
         :messages (if (listp messages) (coerce messages 'vector) messages)
         :tools (if (listp tools) (coerce tools 'vector) tools)
         :context (if (listp context) (coerce context 'vector) context)
         :forwarded-props forwarded-props))

(defun make-ag-ui-agent (&key (name "agent") handler)
  (make-instance 'ag-ui-agent :name name :handler handler))

(defun make-run-started-event (&key thread-id run-id parent-run-id input timestamp)
  (%make 'run-started-event :event-type "RUN_STARTED"
         :thread-id thread-id :run-id run-id
         :parent-run-id parent-run-id :input input :timestamp timestamp))

(defun make-run-finished-event (&key thread-id run-id result timestamp)
  (%make 'run-finished-event :event-type "RUN_FINISHED"
         :thread-id thread-id :run-id run-id :result result :timestamp timestamp))

(defun make-run-error-event (&key message code timestamp)
  (%make 'run-error-event :event-type "RUN_ERROR"
         :message message :code code :timestamp timestamp))

(defun make-step-started-event (&key step-name timestamp)
  (%make 'step-started-event :event-type "STEP_STARTED"
         :step-name step-name :timestamp timestamp))

(defun make-step-finished-event (&key step-name timestamp)
  (%make 'step-finished-event :event-type "STEP_FINISHED"
         :step-name step-name :timestamp timestamp))

(defun make-text-message-start-event (&key message-id (role "assistant") timestamp)
  (%make 'text-message-start-event :event-type "TEXT_MESSAGE_START"
         :message-id message-id :role role :timestamp timestamp))

(defun make-text-message-content-event (&key message-id delta timestamp)
  (%make 'text-message-content-event :event-type "TEXT_MESSAGE_CONTENT"
         :message-id message-id :delta delta :timestamp timestamp))

(defun make-text-message-end-event (&key message-id timestamp)
  (%make 'text-message-end-event :event-type "TEXT_MESSAGE_END"
         :message-id message-id :timestamp timestamp))

(defun make-tool-call-start-event (&key tool-call-id tool-call-name
                                     parent-message-id timestamp)
  (%make 'tool-call-start-event :event-type "TOOL_CALL_START"
         :tool-call-id tool-call-id :tool-call-name tool-call-name
         :parent-message-id parent-message-id :timestamp timestamp))

(defun make-tool-call-args-event (&key tool-call-id delta timestamp)
  (%make 'tool-call-args-event :event-type "TOOL_CALL_ARGS"
         :tool-call-id tool-call-id :delta delta :timestamp timestamp))

(defun make-tool-call-end-event (&key tool-call-id timestamp)
  (%make 'tool-call-end-event :event-type "TOOL_CALL_END"
         :tool-call-id tool-call-id :timestamp timestamp))

(defun make-tool-call-result-event (&key message-id tool-call-id content
                                      (role "tool") timestamp)
  (%make 'tool-call-result-event :event-type "TOOL_CALL_RESULT"
         :message-id message-id :tool-call-id tool-call-id
         :content content :role role :timestamp timestamp))

(defun make-state-snapshot-event (&key snapshot timestamp)
  (%make 'state-snapshot-event :event-type "STATE_SNAPSHOT"
         :snapshot snapshot :timestamp timestamp))

(defun make-state-delta-event (&key delta timestamp)
  (%make 'state-delta-event :event-type "STATE_DELTA"
         :delta (if (listp delta) (coerce delta 'vector) delta)
         :timestamp timestamp))

(defun make-messages-snapshot-event (&key messages timestamp)
  (%make 'messages-snapshot-event :event-type "MESSAGES_SNAPSHOT"
         :messages (if (listp messages) (coerce messages 'vector) messages)
         :timestamp timestamp))
