(in-package #:ag-ui-protocol)

;;; Official field names: https://docs.ag-ui.com/concepts/events
;;; Shape lives in schema-protocol; JSON Schema emit/validate in schema-protocol-json.
;;;
;;; :protobuf is JSON-as-WKT (google.protobuf.Value) via serdes :wkt — not the
;;; official Event oneof. Unknown events survive because the payload is the
;;; dump table. Load protobuf-backend-cl-protobufs to register the format.

(defun %wkt-available-p ()
  (not (null (serdes-protocol:find-backend :wkt nil))))

(defun %ensure-wkt ()
  (unless (%wkt-available-p)
    (error 'ag-ui-error
           :message "protobuf format needs serdes :wkt — load protobuf-backend-cl-protobufs")))

(defun %event-json (event)
  (if (typep event 'unknown-ag-ui-event)
      (or (unknown-ag-ui-event-table event) (stack-schema:dump event))
      (stack-schema:dump event)))

(defun %encode-payload (table format)
  (ecase format
    (:json (encode-json table))
    (:protobuf
     (%ensure-wkt)
     (serdes-protocol:encode table :format :wkt))))

(defun %event-table (source format)
  (cond
    ((hash-table-p source) source)
    ((eq format :protobuf)
     (%ensure-wkt)
     (serdes-protocol:decode
      (if (and (vectorp source) (not (stringp source)))
          source
          (error 'ag-ui-error
                 :message "protobuf decode needs an octet vector"))
      :format :wkt))
    (t (decode-json (%source-string source)))))

(defgeneric encode-ag-ui-event (event &key format)
  (:documentation "Encode EVENT. :json → string; :protobuf → WKT Value octets.")
  (:method ((event ag-ui-event) &key (format :json))
    (%encode-payload (%event-json event) format)))

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
  "WHATWG `data: {json}\\n\\n`. FORMAT must be :json — binary is not SSE."
  (unless (eq format :json)
    (error 'ag-ui-error
           :message "encode-ag-ui-sse is JSON-only; use encode-ag-ui-framed for protobuf"))
  (sse-protocol:encode-sse-event
   (sse-protocol:make-sse-event
    :data (encode-ag-ui-event event :format :json))))

(defun decode-ag-ui-sse-stream (source &key (format :json) on-event)
  "Parse an event-stream of AG-UI JSON events. Returns a list of CLOS events.
   SOURCE may be a stream or a string. ON-EVENT fires as each event is read."
  (unless (eq format :json)
    (error 'ag-ui-error
           :message "decode-ag-ui-sse-stream is JSON-only; use decode-ag-ui-framed"))
  (flet ((collect (src)
           (let ((out '()))
             (sse-protocol:map-sse-events
              (lambda (ev)
                (let ((decoded (decode-ag-ui-event (sse-protocol:sse-event-data ev)
                                                   :format :json)))
                  (when on-event (funcall on-event decoded))
                  (push decoded out)))
              src)
             (nreverse out))))
    (if (stringp source)
        (with-input-from-string (s source)
          (collect s))
        (collect source))))

(defun %write-u32be (vector index n)
  (setf (aref vector index) (ldb (byte 8 24) n)
        (aref vector (+ index 1)) (ldb (byte 8 16) n)
        (aref vector (+ index 2)) (ldb (byte 8 8) n)
        (aref vector (+ index 3)) (ldb (byte 8 0) n)))

(defun %read-u32be (vector index)
  (logior (ash (aref vector index) 24)
          (ash (aref vector (+ index 1)) 16)
          (ash (aref vector (+ index 2)) 8)
          (aref vector (+ index 3))))

(defun encode-ag-ui-framed (event)
  "One length-prefixed WKT Value (big-endian uint32 + protobuf octets)."
  (let* ((payload (encode-ag-ui-event event :format :protobuf))
         (out (make-array (+ 4 (length payload)) :element-type '(unsigned-byte 8))))
    (%write-u32be out 0 (length payload))
    (replace out payload :start1 4)
    out))

(defun map-ag-ui-framed (function octets)
  "Call FUNCTION with each decoded event from length-prefixed OCTETS."
  (let ((i 0)
        (n (length octets)))
    (loop while (< i n)
          do (when (> (+ i 4) n)
               (error 'ag-ui-error :message "truncated protobuf length prefix"))
             (let ((len (%read-u32be octets i)))
               (incf i 4)
               (when (> (+ i len) n)
                 (error 'ag-ui-error :message "truncated protobuf event"))
               (funcall function
                        (decode-ag-ui-event (subseq octets i (+ i len))
                                            :format :protobuf))
               (incf i len))))
  (values))

(defun decode-ag-ui-framed (octets &key on-event)
  "Decode a length-prefixed protobuf event stream to a list of CLOS events."
  (let ((out '()))
    (map-ag-ui-framed (lambda (ev)
                        (when on-event (funcall on-event ev))
                        (push ev out))
                      octets)
    (nreverse out)))
