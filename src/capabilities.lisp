(in-package #:ag-ui-protocol)

;;; AgentCapabilities — discovery, not negotiation. An agent declares what it
;;; supports; a client adapts its UI. Nothing here is agreed between the two.
;;;
;;; Every field is optional and three-valued: absent means "not declared", which
;;; is distinct from an explicit false. `tools.supported = false` says tool
;;; calling is off even though `items` is populated; omitting `tools` says
;;; nothing at all. That is why these are BOOLEAN slots left unbound rather than
;;; defaulting to NIL — DUMP omits unbound slots, so absence survives the wire.

(stack-schema:defschema identity-capabilities ()
  (name string :optional t :accessor %identity-name)
  (type string :optional t :accessor %identity-type)
  (description string :optional t :accessor %identity-description)
  (version string :optional t :accessor %identity-version)
  (provider string :optional t :accessor %identity-provider)
  (documentation-url string :optional t :accessor %identity-documentation-url)
  (metadata hash-table :optional t :accessor %identity-metadata)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema transport-capabilities ()
  (streaming boolean :optional t :accessor %transport-streaming-p)
  (websocket boolean :optional t :accessor %transport-websocket-p)
  (http-binary boolean :optional t :accessor %transport-http-binary-p)
  (push-notifications boolean :optional t :accessor %transport-push-notifications-p)
  (resumable boolean :optional t :accessor %transport-resumable-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema tools-capabilities ()
  (supported boolean :optional t :accessor %tools-supported-p)
  (items (vector ag-ui-tool) :optional t :accessor %tools-items)
  (parallel-calls boolean :optional t :accessor %tools-parallel-calls-p)
  (client-provided boolean :optional t :accessor %tools-client-provided-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema output-capabilities ()
  (structured-output boolean :optional t :accessor %output-structured-output-p)
  (supported-mime-types (vector string) :optional t :accessor %output-mime-types)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema state-capabilities ()
  (snapshots boolean :optional t :accessor %state-snapshots-p)
  (deltas boolean :optional t :accessor %state-deltas-p)
  (memory boolean :optional t :accessor %state-memory-p)
  (persistent-state boolean :optional t :accessor %state-persistent-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema sub-agent-descriptor ()
  (name string :accessor %sub-agent-name)
  (description string :optional t :accessor %sub-agent-description)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema multi-agent-capabilities ()
  (supported boolean :optional t :accessor %multi-agent-supported-p)
  (delegation boolean :optional t :accessor %multi-agent-delegation-p)
  (handoffs boolean :optional t :accessor %multi-agent-handoffs-p)
  (sub-agents (vector sub-agent-descriptor) :optional t :accessor %multi-agent-sub-agents)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema reasoning-capabilities ()
  (supported boolean :optional t :accessor %reasoning-supported-p)
  (streaming boolean :optional t :accessor %reasoning-streaming-p)
  (encrypted boolean :optional t :accessor %reasoning-encrypted-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema multimodal-input-capabilities ()
  (image boolean :optional t :accessor %multimodal-image-p)
  (audio boolean :optional t :accessor %multimodal-audio-p)
  (video boolean :optional t :accessor %multimodal-video-p)
  (pdf boolean :optional t :accessor %multimodal-pdf-p)
  (file boolean :optional t :accessor %multimodal-file-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema multimodal-output-capabilities ()
  (image boolean :optional t :accessor %multimodal-image-p)
  (audio boolean :optional t :accessor %multimodal-audio-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema multimodal-capabilities ()
  (input multimodal-input-capabilities :optional t :accessor %multimodal-input)
  (output multimodal-output-capabilities :optional t :accessor %multimodal-output)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema execution-capabilities ()
  (code-execution boolean :optional t :accessor %execution-code-execution-p)
  (sandboxed boolean :optional t :accessor %execution-sandboxed-p)
  (max-iterations number :optional t :accessor %execution-max-iterations)
  (max-execution-time number :optional t :accessor %execution-max-time)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema human-in-the-loop-capabilities ()
  (supported boolean :optional t :accessor %hitl-supported-p)
  (approvals boolean :optional t :accessor %hitl-approvals-p)
  (interventions boolean :optional t :accessor %hitl-interventions-p)
  (feedback boolean :optional t :accessor %hitl-feedback-p)
  (interrupts boolean :optional t :accessor %hitl-interrupts-p)
  (approve-with-edits boolean :optional t :accessor %hitl-approve-with-edits-p)
  (:key-style :camel)
  (:extra :allow))

(stack-schema:defschema agent-capabilities ()
  (identity identity-capabilities :optional t :accessor %capabilities-identity)
  (transport transport-capabilities :optional t :accessor %capabilities-transport)
  (tools tools-capabilities :optional t :accessor %capabilities-tools)
  (output output-capabilities :optional t :accessor %capabilities-output)
  (state state-capabilities :optional t :accessor %capabilities-state)
  (multi-agent multi-agent-capabilities :optional t :accessor %capabilities-multi-agent)
  (reasoning reasoning-capabilities :optional t :accessor %capabilities-reasoning)
  (multimodal multimodal-capabilities :optional t :accessor %capabilities-multimodal)
  (execution execution-capabilities :optional t :accessor %capabilities-execution)
  (human-in-the-loop human-in-the-loop-capabilities :optional t
                     :accessor %capabilities-human-in-the-loop)
  (custom hash-table :optional t :accessor %capabilities-custom)
  (:key-style :camel)
  (:extra :allow))

(defun make-agent-capabilities (&key identity transport tools output state
                                  multi-agent reasoning multimodal execution
                                  human-in-the-loop custom)
  (%make 'agent-capabilities
         :identity identity :transport transport :tools tools :output output
         :state state :multi-agent multi-agent :reasoning reasoning
         :multimodal multimodal :execution execution
         :human-in-the-loop human-in-the-loop :custom custom))

(defgeneric get-capabilities (agent)
  (:documentation "AGENT-CAPABILITIES for AGENT, or NIL if it declares none.

   Discovery only: the result describes the agent at the moment of the call and
   is not agreed with the caller. NIL is a valid answer and means the same as
   an empty declaration — nothing is claimed.")
  (:method (agent) (declare (ignore agent)) nil))

(defmethod get-capabilities ((agent ag-ui-agent))
  ;; A bare protocol agent can honestly claim only the transport it is served
  ;; over. Anything richer belongs to whatever drives it.
  (make-agent-capabilities
   :identity (%make 'identity-capabilities :name (ag-ui-agent-name agent))
   :transport (%make 'transport-capabilities :streaming t)))

(defun encode-agent-capabilities (capabilities)
  (stack-schema:dump capabilities))

(defun decode-agent-capabilities (source)
  (handler-case
      (stack-schema:parse
       'agent-capabilities
       (if (or (hash-table-p source) (listp source))
           source
           (decode-json (%source-string source))))
    (stack-schema:schema-validation-error (c)
      (%schema-error c))))

;;; Safe readers. Optional capability slots stay unbound so DUMP omits them;
;;; a bare accessor would signal UNBOUND-SLOT and collapse "not declared" into
;;; an error. Same contract as EVENT-FIELD.

(defun capabilities-identity (caps) (event-field caps 'identity))
(defun capabilities-transport (caps) (event-field caps 'transport))
(defun capabilities-tools (caps) (event-field caps 'tools))
(defun capabilities-output (caps) (event-field caps 'output))
(defun capabilities-state (caps) (event-field caps 'state))
(defun capabilities-multi-agent (caps) (event-field caps 'multi-agent))
(defun capabilities-reasoning (caps) (event-field caps 'reasoning))
(defun capabilities-multimodal (caps) (event-field caps 'multimodal))
(defun capabilities-execution (caps) (event-field caps 'execution))
(defun capabilities-human-in-the-loop (caps) (event-field caps 'human-in-the-loop))
(defun capabilities-custom (caps) (event-field caps 'custom))

(defun identity-name (identity) (event-field identity 'name))
(defun identity-type (identity) (event-field identity 'type))
(defun identity-description (identity) (event-field identity 'description))
(defun identity-version (identity) (event-field identity 'version))
(defun identity-provider (identity) (event-field identity 'provider))
(defun identity-documentation-url (identity) (event-field identity 'documentation-url))
(defun identity-metadata (identity) (event-field identity 'metadata))

(defun transport-streaming-p (transport) (event-field transport 'streaming))
(defun transport-websocket-p (transport) (event-field transport 'websocket))
(defun transport-http-binary-p (transport) (event-field transport 'http-binary))
(defun transport-push-notifications-p (transport) (event-field transport 'push-notifications))
(defun transport-resumable-p (transport) (event-field transport 'resumable))

(defun tools-supported-p (tools) (event-field tools 'supported))
(defun tools-items (tools) (event-field tools 'items))
(defun tools-parallel-calls-p (tools) (event-field tools 'parallel-calls))
(defun tools-client-provided-p (tools) (event-field tools 'client-provided))

(defun output-structured-output-p (output) (event-field output 'structured-output))
(defun output-mime-types (output) (event-field output 'supported-mime-types))

(defun state-snapshots-p (state) (event-field state 'snapshots))
(defun state-deltas-p (state) (event-field state 'deltas))
(defun state-memory-p (state) (event-field state 'memory))
(defun state-persistent-p (state) (event-field state 'persistent-state))

(defun multi-agent-supported-p (multi) (event-field multi 'supported))
(defun multi-agent-delegation-p (multi) (event-field multi 'delegation))
(defun multi-agent-handoffs-p (multi) (event-field multi 'handoffs))
(defun multi-agent-sub-agents (multi) (event-field multi 'sub-agents))
(defun sub-agent-name (desc) (event-field desc 'name))
(defun sub-agent-description (desc) (event-field desc 'description))

(defun reasoning-supported-p (reasoning) (event-field reasoning 'supported))
(defun reasoning-streaming-p (reasoning) (event-field reasoning 'streaming))
(defun reasoning-encrypted-p (reasoning) (event-field reasoning 'encrypted))

(defun multimodal-input (multimodal) (event-field multimodal 'input))
(defun multimodal-output (multimodal) (event-field multimodal 'output))
(defun multimodal-image-p (part) (event-field part 'image))
(defun multimodal-audio-p (part) (event-field part 'audio))
(defun multimodal-video-p (part) (event-field part 'video))
(defun multimodal-pdf-p (part) (event-field part 'pdf))
(defun multimodal-file-p (part) (event-field part 'file))

(defun execution-code-execution-p (execution) (event-field execution 'code-execution))
(defun execution-sandboxed-p (execution) (event-field execution 'sandboxed))
(defun execution-max-iterations (execution) (event-field execution 'max-iterations))
(defun execution-max-time (execution) (event-field execution 'max-execution-time))

(defun hitl-supported-p (hitl) (event-field hitl 'supported))
(defun hitl-approvals-p (hitl) (event-field hitl 'approvals))
(defun hitl-interventions-p (hitl) (event-field hitl 'interventions))
(defun hitl-feedback-p (hitl) (event-field hitl 'feedback))
(defun hitl-interrupts-p (hitl) (event-field hitl 'interrupts))
(defun hitl-approve-with-edits-p (hitl) (event-field hitl 'approve-with-edits))
