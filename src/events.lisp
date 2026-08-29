(in-package #:ag-ui-protocol)

;;; Official field names: https://docs.ag-ui.com/concepts/events
;;; Shape lives in schema-protocol; JSON Schema emit/validate in schema-protocol-json.

(defun %encode-payload (json format)
  (ecase format
    (:json json)
    (:protobuf (babel:string-to-octets json :encoding :utf-8))))

(defun %event-table (source format)
  (cond
    ((hash-table-p source) source)
    ((eq format :protobuf)
     (decode-json
      (if (and (vectorp source) (not (stringp source)))
          (babel:octets-to-string source :encoding :utf-8)
          (%source-string source))))
    (t (decode-json (%source-string source)))))

(defgeneric encode-ag-ui-event (event &key format)
  (:documentation "Encode EVENT. :json → string; :protobuf → UTF-8 octets of the JSON
   (official Event proto is not compiled yet — same payload, different container).")
  (:method ((event ag-ui-event) &key (format :json))
    (%encode-payload (encode-json (stack-schema:dump event)) format))
  (:method ((event unknown-ag-ui-event) &key (format :json))
    ;; Forward the source table verbatim: a relay must not silently drop fields
    ;; of an event type it does not model.
    (%encode-payload (encode-json (or (unknown-ag-ui-event-table event)
                                      (stack-schema:dump event)))
                     format)))

(defun known-event-type-p (type)
  "Does this build model TYPE? Derived from AG-UI-EVENT's declared :tag variants."
  (and (stringp type)
       (not (null (stack-schema:schema-variant 'ag-ui-event type)))))

(defgeneric decode-ag-ui-event (source &key format strict)
  (:documentation "Decode one AG-UI event.

   An unrecognized `type` yields an UNKNOWN-AG-UI-EVENT holding the source table
   rather than signalling: the spec requires consumers to tolerate events from
   newer producers instead of failing the stream. Pass :STRICT T to signal
   AG-UI-ERROR instead, for callers policing their own output.")
  (:method (source &key (format :json) strict)
    (let* ((obj (%event-table source format))
           (type (param obj "type")))
      (if (or strict (known-event-type-p type))
          (handler-case
              (stack-schema:parse 'ag-ui-event obj)
            (stack-schema:schema-validation-error (c)
              (%schema-error c)))
          (make-unknown-ag-ui-event
           :event-type (if (stringp type) type "")
           :raw-table (if (hash-table-p obj) obj (json-object))
           :timestamp (let ((ts (param obj "timestamp")))
                        (and (numberp ts) ts)))))))

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
   SOURCE may be a stream or a string. ON-EVENT fires as each event is read."
  (flet ((collect (src)
           (let ((out '()))
             (sse-protocol:map-sse-events
              (lambda (ev)
                (let ((decoded (decode-ag-ui-event (sse-protocol:sse-event-data ev)
                                                   :format format)))
                  (when on-event (funcall on-event decoded))
                  (push decoded out)))
              src)
             (nreverse out))))
    (if (stringp source)
        (with-input-from-string (s source)
          (collect s))
        (collect source))))
