(in-package #:ag-ui-protocol/capability)

;;; Outbound: catalogue → AgentCapabilities.
;;;
;;; Most of the document is derivable. llm-protocol already computes these from
;;; BACKEND-SUPPORTS-P and registers a marker capability per supported feature,
;;; so an agent backed by one can declare its capabilities instead of having
;;; them hand-maintained and drifting.

(defparameter +llm-capability-map+
  '((:llm-tools              :tools :supported)
    (:llm-thinking           :reasoning :supported)
    (:llm-structured-output  :output :structured-output)
    (:llm-vision             :multimodal-input :image)
    (:llm-audio              :multimodal-input :audio)
    (:llm-video              :multimodal-input :video)
    (:llm-files              :multimodal-input :file)
    (:llm-transcription      :multimodal-input :audio)
    (:llm-speech             :multimodal-output :audio))
  "capability-protocol name → (AgentCapabilities category, field).

   Only presence carries over. A capability that is not registered leaves the
   field absent rather than setting it false, because \"not declared\" is what
   we actually know — capability-protocol cannot distinguish an unsupported
   feature from an unmentioned one.")

(defun %supported (host name)
  (and host (cap:capability-supported-p host name)))

(defun %any (host names)
  (some (lambda (name) (%supported host name)) names))

(defun %fields (host category)
  "Plist of AgentCapabilities fields in CATEGORY that HOST supports."
  (loop for (name cat field) in +llm-capability-map+
        when (and (eq cat category) (%supported host name))
          append (list (intern (symbol-name field) :keyword) t)))

(defun %maybe (class fields)
  "Build CLASS from FIELDS, or NIL when nothing is declared — an empty category
   object would claim more than we know."
  (when fields (apply #'make-instance class fields)))

(defun capabilities-from-catalogue (host &key identity execution
                                           human-in-the-loop multi-agent
                                           state transport custom)
  "Derive AGENT-CAPABILITIES from a capability catalogue or blackboard HOST.

   Only the LLM-shaped fields are derivable; anything the catalogue has no
   vocabulary for — identity, execution limits, human-in-the-loop, multi-agent
   topology — is passed in by the caller, which does know."
  (let* ((multimodal-input (%fields host :multimodal-input))
         (multimodal-output (%fields host :multimodal-output))
         (multimodal
           (when (or multimodal-input multimodal-output)
             (make-instance
              'ag-ui:multimodal-capabilities
              :input (%maybe 'ag-ui:multimodal-input-capabilities multimodal-input)
              :output (%maybe 'ag-ui:multimodal-output-capabilities multimodal-output)))))
    (ag-ui:make-agent-capabilities
     :identity identity
     :transport (or transport
                    (%maybe 'ag-ui:transport-capabilities
                            (when (%any host '(:llm-generation)) '(:streaming t))))
     :tools (%maybe 'ag-ui:tools-capabilities (%fields host :tools))
     :output (%maybe 'ag-ui:output-capabilities (%fields host :output))
     :state state
     :multi-agent multi-agent
     :reasoning (%maybe 'ag-ui:reasoning-capabilities (%fields host :reasoning))
     :multimodal multimodal
     :execution execution
     :human-in-the-loop human-in-the-loop
     :custom custom)))

;;; Inbound: AgentCapabilities → catalogue.
;;;
;;; The more interesting direction. Registering a remote agent's declared
;;; features as marker capabilities lets a planner query CAPABILITY-SUPPORTED-P
;;; uniformly, without caring whether the provider is a local backend or an
;;; AG-UI agent on the other end of an HTTP connection.
;;;
;;; Lossy on the way in: tri-state collapses to presence, and scalars such as
;;; execution.maxIterations have nowhere to go. Read the document itself when
;;; those matter.

(defun %declared-p (object accessor)
  (and object (and (funcall accessor object) t)))

(defun register-agent-capabilities (host capabilities)
  "Register marker capabilities on HOST for everything CAPABILITIES declares.
   Returns the list of registered names."
  (let ((registered '()))
    (flet ((mark (name class)
             (cap:register-capability host (make-instance class))
             (push name registered)))
      (let ((tools (ag-ui:capabilities-tools capabilities))
            (reasoning (ag-ui:capabilities-reasoning capabilities))
            (output (ag-ui:capabilities-output capabilities))
            (multimodal (ag-ui:capabilities-multimodal capabilities)))
        (when (%declared-p tools #'ag-ui:tools-supported-p)
          (mark :llm-tools 'cap:llm-tools-capability))
        (when (%declared-p reasoning #'ag-ui:reasoning-supported-p)
          (mark :llm-thinking 'cap:llm-thinking-capability))
        (when (%declared-p output #'ag-ui:output-structured-output-p)
          (mark :llm-structured-output 'cap:llm-structured-output-capability))
        (let ((input (and multimodal (ag-ui:multimodal-input multimodal)))
              (out (and multimodal (ag-ui:multimodal-output multimodal))))
          (when (%declared-p input #'ag-ui:multimodal-image-p)
            (mark :llm-vision 'cap:llm-vision-capability))
          (when (%declared-p input #'ag-ui:multimodal-audio-p)
            (mark :llm-audio 'cap:llm-audio-capability))
          (when (%declared-p input #'ag-ui:multimodal-video-p)
            (mark :llm-video 'cap:llm-video-capability))
          (when (%declared-p input #'ag-ui:multimodal-file-p)
            (mark :llm-files 'cap:llm-files-capability))
          (when (%declared-p out #'ag-ui:multimodal-audio-p)
            (mark :llm-speech 'cap:llm-speech-capability)))))
    (nreverse registered)))
