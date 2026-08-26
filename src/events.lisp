(in-package #:ag-ui-protocol)

;;; Official field names: https://docs.ag-ui.com/concepts/events
;;; Re-expressed MIT — not copied from @ag-ui/core sources.

(defun %base-json (event &rest kvs)
  (apply #'json-object
         "type" (ag-ui-event-type event)
         "timestamp" (or (ag-ui-event-timestamp event) :omit)
         "rawEvent" (or (ag-ui-event-raw-event event) :omit)
         "metadata" (or (ag-ui-event-metadata event) :omit)
         kvs))

(defgeneric %event-json (event)
  (:method ((event run-started-event))
    (%base-json event
                "threadId" (run-started-thread-id event)
                "runId" (run-started-run-id event)
                "parentRunId" (or (run-started-parent-run-id event) :omit)
                "input" (if (run-started-input event)
                            (encode-run-agent-input (run-started-input event))
                            :omit)))
  (:method ((event run-finished-event))
    (%base-json event
                "threadId" (run-finished-thread-id event)
                "runId" (run-finished-run-id event)
                "result" (or (run-finished-result event) :omit)))
  (:method ((event run-error-event))
    (%base-json event
                "message" (run-error-message event)
                "code" (or (run-error-code event) :omit)))
  (:method ((event step-started-event))
    (%base-json event "stepName" (step-event-name event)))
  (:method ((event step-finished-event))
    (%base-json event "stepName" (step-event-name event)))
  (:method ((event text-message-start-event))
    (%base-json event
                "messageId" (text-message-id event)
                "role" (or (text-message-role event) "assistant")))
  (:method ((event text-message-content-event))
    (%base-json event
                "messageId" (text-message-id event)
                "delta" (text-message-delta event)))
  (:method ((event text-message-end-event))
    (%base-json event "messageId" (text-message-id event)))
  (:method ((event tool-call-start-event))
    (%base-json event
                "toolCallId" (tool-call-id event)
                "toolCallName" (tool-call-name event)
                "parentMessageId" (or (tool-call-parent-message-id event) :omit)))
  (:method ((event tool-call-args-event))
    (%base-json event
                "toolCallId" (tool-call-id event)
                "delta" (tool-call-delta event)))
  (:method ((event tool-call-end-event))
    (%base-json event "toolCallId" (tool-call-id event)))
  (:method ((event tool-call-result-event))
    (%base-json event
                "messageId" (text-message-id event)
                "toolCallId" (tool-call-id event)
                "content" (tool-call-result-content event)
                "role" (or (tool-call-result-role event) :omit)))
  (:method ((event state-snapshot-event))
    (%base-json event "snapshot" (state-snapshot-value event)))
  (:method ((event state-delta-event))
    (%base-json event "delta" (%as-vector (state-delta-patch event))))
  (:method ((event messages-snapshot-event))
    (%base-json event
                "messages" (map 'vector #'encode-message
                                (or (messages-snapshot-messages event) #())))))

(defun %decode-input-field (obj)
  (let ((in (param obj "input")))
    (when (hash-table-p in)
      (decode-run-agent-input in))))

(defun %decode-event-object (obj)
  (let ((type (param obj "type"))
        (ts (param obj "timestamp"))
        (raw (param obj "rawEvent"))
        (meta (param obj "metadata")))
    (flet ((stamp (event)
             (setf (ag-ui-event-timestamp event) ts
                   (ag-ui-event-raw-event event) raw
                   (ag-ui-event-metadata event) meta)
             event))
      (stamp
       (cond
         ((string= type "RUN_STARTED")
          (make-run-started-event
           :thread-id (param obj "threadId")
           :run-id (param obj "runId")
           :parent-run-id (param obj "parentRunId")
           :input (%decode-input-field obj)))
         ((string= type "RUN_FINISHED")
          (make-run-finished-event
           :thread-id (param obj "threadId")
           :run-id (param obj "runId")
           :result (param obj "result")))
         ((string= type "RUN_ERROR")
          (make-run-error-event
           :message (param obj "message")
           :code (param obj "code")))
         ((string= type "STEP_STARTED")
          (make-step-started-event :step-name (param obj "stepName")))
         ((string= type "STEP_FINISHED")
          (make-step-finished-event :step-name (param obj "stepName")))
         ((string= type "TEXT_MESSAGE_START")
          (make-text-message-start-event
           :message-id (param obj "messageId")
           :role (or (param obj "role") "assistant")))
         ((string= type "TEXT_MESSAGE_CONTENT")
          (make-text-message-content-event
           :message-id (param obj "messageId")
           :delta (param obj "delta")))
         ((string= type "TEXT_MESSAGE_END")
          (make-text-message-end-event :message-id (param obj "messageId")))
         ((string= type "TOOL_CALL_START")
          (make-tool-call-start-event
           :tool-call-id (param obj "toolCallId")
           :tool-call-name (param obj "toolCallName")
           :parent-message-id (param obj "parentMessageId")))
         ((string= type "TOOL_CALL_ARGS")
          (make-tool-call-args-event
           :tool-call-id (param obj "toolCallId")
           :delta (param obj "delta")))
         ((string= type "TOOL_CALL_END")
          (make-tool-call-end-event :tool-call-id (param obj "toolCallId")))
         ((string= type "TOOL_CALL_RESULT")
          (make-tool-call-result-event
           :message-id (param obj "messageId")
           :tool-call-id (param obj "toolCallId")
           :content (param obj "content")
           :role (or (param obj "role") "tool")))
         ((string= type "STATE_SNAPSHOT")
          (make-state-snapshot-event :snapshot (param obj "snapshot")))
         ((string= type "STATE_DELTA")
          (make-state-delta-event :delta (%as-list (param obj "delta"))))
         ((string= type "MESSAGES_SNAPSHOT")
          (make-messages-snapshot-event
           :messages (mapcar #'decode-message (%as-list (param obj "messages")))))
         (t
          (error 'ag-ui-error
                 :message (format nil "unknown AG-UI event type ~s" type))))))))

(defgeneric encode-ag-ui-event (event &key format)
  (:documentation "Encode EVENT. :json → string; :protobuf → UTF-8 octets of the JSON
   (official Event proto is not compiled yet — same payload, different container).")
  (:method ((event ag-ui-event) &key (format :json))
    (let ((json (encode-json (%event-json event))))
      (ecase format
        (:json json)
        (:protobuf (babel:string-to-octets json :encoding :utf-8))))))

(defgeneric decode-ag-ui-event (source &key format)
  (:method (source &key (format :json))
    (let* ((obj (if (hash-table-p source)
                    source
                    (ecase format
                      (:json (decode-json (%source-string source)))
                      (:protobuf
                       (decode-json
                        (if (and (vectorp source) (not (stringp source)))
                            (babel:octets-to-string source :encoding :utf-8)
                            (%source-string source))))))))
      (%decode-event-object obj))))

(defun encode-ag-ui-sse (event &key (format :json))
  "WHATWG `data: {json}\\n\\n` (or UTF-8 JSON octets as data for :protobuf)."
  (sse-protocol:encode-sse-event
   (sse-protocol:make-sse-event
    :data (if (eq format :json)
              (encode-ag-ui-event event :format :json)
              (babel:octets-to-string
               (encode-ag-ui-event event :format :protobuf)
               :encoding :utf-8)))))

(defun decode-ag-ui-sse-stream (source &key (format :json) on-event)
  "Parse an event-stream of AG-UI JSON events. Returns a list of CLOS events.
   SOURCE may be a stream or a string."
  (flet ((collect (src)
           (let ((out '()))
             (dolist (ev (sse-protocol:collect-sse-events src))
               (let ((decoded (decode-ag-ui-event (sse-protocol:sse-event-data ev)
                                                  :format format)))
                 (when on-event (funcall on-event decoded))
                 (push decoded out)))
             (nreverse out))))
    (if (stringp source)
        (with-input-from-string (s source)
          (collect s))
        (collect source))))
