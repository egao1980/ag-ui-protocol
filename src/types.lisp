(in-package #:ag-ui-protocol)

;;; Interchange models = schema-protocol (API payloads, not persistence).
;;; Wire keys = camelCase to match @ag-ui/core. Event dispatch = :tag type.

(defun %make (class &rest keys)
  "MAKE-INSTANCE dropping NIL optionals so DUMP omits them."
  (apply #'make-instance class
         (loop for (k v) on keys by #'cddr
               when v
                 append (list k v))))

(defun event-field (event slot-name)
  "Value of SLOT-NAME on EVENT, or NIL when the field was omitted.

   The inverse of %MAKE. Optional fields are deliberately left unbound so DUMP
   omits them rather than emitting null, which makes a bare accessor call unsafe
   on any field a producer may skip. Reducers and transforms should read
   optional fields through this."
  (and (slot-exists-p event slot-name)
       (slot-boundp event slot-name)
       (slot-value event slot-name)))

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

;;; Variants are listed explicitly rather than discovered from subclasses so
;;; UNKNOWN-AG-UI-EVENT — which accepts any `type` — can subclass AG-UI-EVENT
;;; for dispatch without shadowing a real variant during tag resolution.
;;; Adding a wire event means adding it here.
;;; SUBAGENT-RUN-ID sits on the base rather than being repeated on the ~25 events
;;; that declare it upstream. It is optional everywhere, so DUMP omits it and the
;;; wire is unchanged; the only divergence is accepting it on the deprecated
;;; THINKING_* family, which upstream rejects — tolerant in, correct out.
(stack-schema:defschema ag-ui-event ()
  (event-type string :key "type" :accessor ag-ui-event-type)
  (timestamp number :optional t :accessor ag-ui-event-timestamp)
  (raw-event t :optional t :accessor ag-ui-event-raw-event)
  (metadata hash-table :optional t :accessor ag-ui-event-metadata)
  (subagent-run-id string :optional t :accessor ag-ui-event-subagent-run-id)
  (:tag event-type
        run-started-event run-finished-event run-error-event
        step-started-event step-finished-event
        text-message-start-event text-message-content-event
        text-message-end-event text-message-chunk-event
        tool-call-start-event tool-call-args-event tool-call-end-event
        tool-call-result-event tool-call-chunk-event
        state-snapshot-event state-delta-event messages-snapshot-event
        activity-snapshot-event activity-delta-event
        subagent-started-event subagent-finished-event subagent-error-event
        reasoning-start-event reasoning-end-event
        reasoning-message-start-event reasoning-message-content-event
        reasoning-message-end-event reasoning-message-chunk-event
        reasoning-encrypted-value-event
        thinking-start-event thinking-end-event
        thinking-text-message-start-event thinking-text-message-content-event
        thinking-text-message-end-event
        raw-event custom-event)
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

;;; Convenience chunk events. A producer may send these instead of the explicit
;;; triads; EXPAND-AG-UI-CHUNKS in chunks.lisp turns them back into START /
;;; CONTENT / END so reducers only ever see one shape.
(stack-schema:defschema text-message-chunk-event (ag-ui-event)
  (event-type (eql "TEXT_MESSAGE_CHUNK") :default "TEXT_MESSAGE_CHUNK" :key "type")
  (message-id string :optional t :accessor text-message-id)
  (role string :optional t :accessor text-message-role)
  (delta string :optional t :accessor text-message-delta)
  (name string :optional t :accessor text-message-name)
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

(stack-schema:defschema tool-call-chunk-event (ag-ui-event)
  (event-type (eql "TOOL_CALL_CHUNK") :default "TOOL_CALL_CHUNK" :key "type")
  (tool-call-id string :optional t :accessor tool-call-id)
  (tool-call-name string :optional t :accessor tool-call-name)
  (parent-message-id string :optional t :accessor tool-call-parent-message-id)
  (delta string :optional t :accessor tool-call-delta)
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

;;; Activity — structured, in-progress work reported between chat messages,
;;; following the same snapshot/delta shape as state. An activity message shares
;;; the message id space, so its id must not collide with a text message.

(stack-schema:defschema activity-snapshot-event (ag-ui-event)
  (event-type (eql "ACTIVITY_SNAPSHOT") :default "ACTIVITY_SNAPSHOT" :key "type")
  (message-id string :accessor text-message-id)
  (activity-type string :accessor activity-type)
  (content hash-table :accessor activity-content)
  (replace boolean :optional t :accessor activity-replace-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema activity-delta-event (ag-ui-event)
  (event-type (eql "ACTIVITY_DELTA") :default "ACTIVITY_DELTA" :key "type")
  (message-id string :accessor text-message-id)
  (activity-type string :accessor activity-type)
  (patch (vector hash-table) :accessor activity-patch)
  (:key-style :camel)
  (:extra :allow))

;;; Subagents — bracket a delegated child run so a frontend can tell which
;;; subagent produced which output. Attribution on other events rides on the
;;; base AG-UI-EVENT-SUBAGENT-RUN-ID.

(stack-schema:defschema subagent-started-event (ag-ui-event)
  (event-type (eql "SUBAGENT_STARTED") :default "SUBAGENT_STARTED" :key "type")
  (subagent-run-id string :accessor ag-ui-event-subagent-run-id)
  (name string :accessor subagent-name)
  (description string :optional t :accessor subagent-description)
  (parent-subagent-run-id string :optional t :accessor subagent-parent-run-id)
  (parent-tool-call-id string :optional t :accessor subagent-parent-tool-call-id)
  (parent-message-id string :optional t :accessor subagent-parent-message-id)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema subagent-finished-event (ag-ui-event)
  (event-type (eql "SUBAGENT_FINISHED") :default "SUBAGENT_FINISHED" :key "type")
  (subagent-run-id string :accessor ag-ui-event-subagent-run-id)
  (result t :optional t :accessor subagent-result)
  (outcome hash-table :optional t :accessor subagent-outcome)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema subagent-error-event (ag-ui-event)
  (event-type (eql "SUBAGENT_ERROR") :default "SUBAGENT_ERROR" :key "type")
  (subagent-run-id string :accessor ag-ui-event-subagent-run-id)
  (message string :accessor run-error-message)
  (code string :optional t :accessor run-error-code)
  (:key-style :camel)
  (:extra :allow))

;;; Reasoning. REASONING_START / REASONING_END bracket a reasoning context;
;;; REASONING_MESSAGE_* stream the portion meant to be shown to a user.
;;; REASONING_ENCRYPTED_VALUE carries opaque chain-of-thought a client stores
;;; and echoes back untouched, for zero-data-retention providers.

(stack-schema:defschema reasoning-start-event (ag-ui-event)
  (event-type (eql "REASONING_START") :default "REASONING_START" :key "type")
  (message-id string :accessor text-message-id)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema reasoning-end-event (ag-ui-event)
  (event-type (eql "REASONING_END") :default "REASONING_END" :key "type")
  (message-id string :accessor text-message-id)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema reasoning-message-start-event (ag-ui-event)
  (event-type (eql "REASONING_MESSAGE_START") :default "REASONING_MESSAGE_START"
              :key "type")
  (message-id string :accessor text-message-id)
  (role (eql "reasoning") :default "reasoning" :accessor text-message-role)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema reasoning-message-content-event (ag-ui-event)
  (event-type (eql "REASONING_MESSAGE_CONTENT")
              :default "REASONING_MESSAGE_CONTENT" :key "type")
  (message-id string :accessor text-message-id)
  (delta string :accessor text-message-delta)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema reasoning-message-end-event (ag-ui-event)
  (event-type (eql "REASONING_MESSAGE_END") :default "REASONING_MESSAGE_END"
              :key "type")
  (message-id string :accessor text-message-id)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema reasoning-message-chunk-event (ag-ui-event)
  (event-type (eql "REASONING_MESSAGE_CHUNK") :default "REASONING_MESSAGE_CHUNK"
              :key "type")
  (message-id string :optional t :accessor text-message-id)
  (delta string :optional t :accessor text-message-delta)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema reasoning-encrypted-value-event (ag-ui-event)
  (event-type (eql "REASONING_ENCRYPTED_VALUE")
              :default "REASONING_ENCRYPTED_VALUE" :key "type")
  (subtype (member "message" "tool-call") :accessor reasoning-encrypted-subtype)
  (entity-id string :accessor reasoning-encrypted-entity-id)
  (encrypted-value string :accessor reasoning-encrypted-value)
  (:key-style :camel)
  (:extra :allow))

;;; Deprecated upstream in favour of REASONING_*, removed at their 1.0. Decoded
;;; so streams from producers that have not migrated still parse.

(stack-schema:defschema thinking-start-event (ag-ui-event)
  (event-type (eql "THINKING_START") :default "THINKING_START" :key "type")
  (title string :optional t :accessor thinking-title)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema thinking-end-event (ag-ui-event)
  (event-type (eql "THINKING_END") :default "THINKING_END" :key "type")
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema thinking-text-message-start-event (ag-ui-event)
  (event-type (eql "THINKING_TEXT_MESSAGE_START")
              :default "THINKING_TEXT_MESSAGE_START" :key "type")
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema thinking-text-message-content-event (ag-ui-event)
  (event-type (eql "THINKING_TEXT_MESSAGE_CONTENT")
              :default "THINKING_TEXT_MESSAGE_CONTENT" :key "type")
  (delta string :accessor text-message-delta)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema thinking-text-message-end-event (ag-ui-event)
  (event-type (eql "THINKING_TEXT_MESSAGE_END")
              :default "THINKING_TEXT_MESSAGE_END" :key "type")
  (:key-style :camel)
  (:extra :allow))

;;; RAW and CUSTOM are the spec's designated extension points: RAW wraps a
;;; foreign system's event verbatim, CUSTOM carries an application-defined one.
;;; The RAW-EVENT class is the wire event of type "RAW"; AG-UI-EVENT-RAW-EVENT
;;; is the unrelated `rawEvent` provenance field every event may carry.

(stack-schema:defschema raw-event (ag-ui-event)
  (event-type (eql "RAW") :default "RAW" :key "type")
  (event t :accessor raw-event-payload)
  (source string :optional t :accessor raw-event-source)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema custom-event (ag-ui-event)
  (event-type (eql "CUSTOM") :default "CUSTOM" :key "type")
  (name string :accessor custom-event-name)
  (value t :optional t :accessor custom-event-value)
  (:key-style :camel)
  (:extra :allow))

;;; Forward compatibility. A producer on a newer spec revision may send event
;;; types this build does not model; the spec requires consumers to tolerate
;;; them rather than fail the stream. DECODE-AG-UI-EVENT parks those here with
;;; the source table intact so ENCODE-AG-UI-EVENT can forward them byte-faithful.
;;; Deliberately absent from AG-UI-EVENT's :tag variants — it is never matched
;;; by tag resolution, only constructed explicitly.
(stack-schema:defschema unknown-ag-ui-event (ag-ui-event)
  (raw-table hash-table :optional t :wire nil :dump nil
             :accessor unknown-ag-ui-event-table)
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

(defun make-activity-snapshot-event (&key message-id activity-type content
                                       (replace t) timestamp)
  (%make 'activity-snapshot-event :event-type "ACTIVITY_SNAPSHOT"
         :message-id message-id :activity-type activity-type
         :content content :replace replace :timestamp timestamp))

(defun make-activity-delta-event (&key message-id activity-type patch timestamp)
  (%make 'activity-delta-event :event-type "ACTIVITY_DELTA"
         :message-id message-id :activity-type activity-type
         :patch (if (listp patch) (coerce patch 'vector) patch)
         :timestamp timestamp))

(defun make-subagent-started-event (&key subagent-run-id name description
                                      parent-subagent-run-id parent-tool-call-id
                                      parent-message-id timestamp)
  (%make 'subagent-started-event :event-type "SUBAGENT_STARTED"
         :subagent-run-id subagent-run-id :name name :description description
         :parent-subagent-run-id parent-subagent-run-id
         :parent-tool-call-id parent-tool-call-id
         :parent-message-id parent-message-id :timestamp timestamp))

(defun make-subagent-finished-event (&key subagent-run-id result outcome timestamp)
  (%make 'subagent-finished-event :event-type "SUBAGENT_FINISHED"
         :subagent-run-id subagent-run-id :result result :outcome outcome
         :timestamp timestamp))

(defun make-subagent-error-event (&key subagent-run-id message code timestamp)
  (%make 'subagent-error-event :event-type "SUBAGENT_ERROR"
         :subagent-run-id subagent-run-id :message message :code code
         :timestamp timestamp))

(defun make-text-message-chunk-event (&key message-id role delta name timestamp)
  (%make 'text-message-chunk-event :event-type "TEXT_MESSAGE_CHUNK"
         :message-id message-id :role role :delta delta :name name
         :timestamp timestamp))

(defun make-tool-call-chunk-event (&key tool-call-id tool-call-name
                                     parent-message-id delta timestamp)
  (%make 'tool-call-chunk-event :event-type "TOOL_CALL_CHUNK"
         :tool-call-id tool-call-id :tool-call-name tool-call-name
         :parent-message-id parent-message-id :delta delta :timestamp timestamp))

(defun make-reasoning-start-event (&key message-id timestamp)
  (%make 'reasoning-start-event :event-type "REASONING_START"
         :message-id message-id :timestamp timestamp))

(defun make-reasoning-end-event (&key message-id timestamp)
  (%make 'reasoning-end-event :event-type "REASONING_END"
         :message-id message-id :timestamp timestamp))

(defun make-reasoning-message-start-event (&key message-id timestamp)
  (%make 'reasoning-message-start-event :event-type "REASONING_MESSAGE_START"
         :message-id message-id :role "reasoning" :timestamp timestamp))

(defun make-reasoning-message-content-event (&key message-id delta timestamp)
  (%make 'reasoning-message-content-event :event-type "REASONING_MESSAGE_CONTENT"
         :message-id message-id :delta delta :timestamp timestamp))

(defun make-reasoning-message-end-event (&key message-id timestamp)
  (%make 'reasoning-message-end-event :event-type "REASONING_MESSAGE_END"
         :message-id message-id :timestamp timestamp))

(defun make-reasoning-message-chunk-event (&key message-id delta timestamp)
  (%make 'reasoning-message-chunk-event :event-type "REASONING_MESSAGE_CHUNK"
         :message-id message-id :delta delta :timestamp timestamp))

(defun make-reasoning-encrypted-value-event (&key subtype entity-id
                                               encrypted-value timestamp)
  (%make 'reasoning-encrypted-value-event :event-type "REASONING_ENCRYPTED_VALUE"
         :subtype subtype :entity-id entity-id
         :encrypted-value encrypted-value :timestamp timestamp))

(defun make-thinking-start-event (&key title timestamp)
  (%make 'thinking-start-event :event-type "THINKING_START"
         :title title :timestamp timestamp))

(defun make-thinking-end-event (&key timestamp)
  (%make 'thinking-end-event :event-type "THINKING_END" :timestamp timestamp))

(defun make-raw-event (&key event source timestamp)
  (%make 'raw-event :event-type "RAW"
         :event event :source source :timestamp timestamp))

(defun make-custom-event (&key name value timestamp)
  (%make 'custom-event :event-type "CUSTOM"
         :name name :value value :timestamp timestamp))

(defun make-unknown-ag-ui-event (&key event-type raw-table timestamp)
  (%make 'unknown-ag-ui-event :event-type event-type
         :raw-table raw-table :timestamp timestamp))
