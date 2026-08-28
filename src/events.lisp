(in-package #:ag-ui-protocol)

;;; Official field names: https://docs.ag-ui.com/concepts/events
;;; Shape lives in schema-protocol; JSON Schema emit/validate in schema-protocol-json.

(defgeneric encode-ag-ui-event (event &key format)
  (:documentation "Encode EVENT. :json → string; :protobuf → UTF-8 octets of the JSON
   (official Event proto is not compiled yet — same payload, different container).")
  (:method ((event ag-ui-event) &key (format :json))
    (let ((json (encode-json (stack-schema:dump event))))
      (ecase format
        (:json json)
        (:protobuf (babel:string-to-octets json :encoding :utf-8))))))

(defgeneric decode-ag-ui-event (source &key format)
  (:method (source &key (format :json))
    (let ((obj (cond
                 ((hash-table-p source) source)
                 ((eq format :protobuf)
                  (decode-json
                   (if (and (vectorp source) (not (stringp source)))
                       (babel:octets-to-string source :encoding :utf-8)
                       (%source-string source))))
                 (t (decode-json (%source-string source))))))
      (handler-case
          (stack-schema:parse 'ag-ui-event obj)
        (stack-schema:schema-validation-error (c)
          (%schema-error c))))))

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
