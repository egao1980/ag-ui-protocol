(in-package #:ag-ui-protocol)

;;; Official @ag-ui/proto Event oneof. Generated classes live in
;;; CL-PROTOBUFS.AG-UI (vendored proto/*.lisp via cl-protobufs protoc).
;;; 21 oneof fields. The other 15 tagged schema classes have no wire field.

(defpackage #:ag-ui-protocol/oneof
  (:use #:cl)
  (:local-nicknames (#:pb #:cl-protobufs.ag-ui)
                    (#:google #:cl-protobufs.google.protobuf)))

(in-package #:ag-ui-protocol/oneof)

(defun %plist-when (&rest keys)
  (loop for (k v) on keys by #'cddr
        when v
          append (list k v)))

(defun %table-to-struct (table)
  (when (hash-table-p table)
    (let ((value (protobuf-protocol:lisp-to-wkt table)))
      (when (cl-protobufs:has-field value 'google:struct-value)
        (google:value.struct-value value)))))

(defun %struct-to-table (struct)
  (when struct
    (protobuf-protocol:wkt-to-lisp
     (google:make-value :struct-value struct))))

(defun %lisp-to-value (value)
  (and value (protobuf-protocol:lisp-to-wkt value)))

(defun %value-to-lisp (value)
  (and value (protobuf-protocol:wkt-to-lisp value)))

(defparameter *event-type-enum*
  '(("TEXT_MESSAGE_START" . :text-message-start)
    ("TEXT_MESSAGE_CONTENT" . :text-message-content)
    ("TEXT_MESSAGE_END" . :text-message-end)
    ("TOOL_CALL_START" . :tool-call-start)
    ("TOOL_CALL_ARGS" . :tool-call-args)
    ("TOOL_CALL_END" . :tool-call-end)
    ("STATE_SNAPSHOT" . :state-snapshot)
    ("STATE_DELTA" . :state-delta)
    ("MESSAGES_SNAPSHOT" . :messages-snapshot)
    ("RAW" . :raw)
    ("CUSTOM" . :custom)
    ("RUN_STARTED" . :run-started)
    ("RUN_FINISHED" . :run-finished)
    ("RUN_ERROR" . :run-error)
    ("STEP_STARTED" . :step-started)
    ("STEP_FINISHED" . :step-finished)
    ("SUBAGENT_STARTED" . :subagent-started)
    ("SUBAGENT_FINISHED" . :subagent-finished)
    ("SUBAGENT_ERROR" . :subagent-error)))

(defun %enum-type (type-string)
  (cdr (assoc type-string *event-type-enum* :test #'string=)))

(defun %type-string (keyword)
  (car (rassoc keyword *event-type-enum* :test #'eq)))

(defun %slot (event name)
  (ag-ui-protocol:event-field event (find-symbol name :ag-ui-protocol)))

(defun %as-list (seq)
  (cond
    ((null seq) nil)
    ((vectorp seq) (coerce seq 'list))
    ((listp seq) seq)
    (t (list seq))))

(defun %pb-field (msg name)
  (when msg
    (let ((sym (find-symbol
                (format nil "~A.~A" (symbol-name (class-name (class-of msg))) name)
                :cl-protobufs.ag-ui)))
      (when (and sym (fboundp sym))
        (funcall sym msg)))))

(defun %sid (event)
  (%slot event "SUBAGENT-RUN-ID"))

(defun %base-event (event)
  (apply #'pb:make-base-event
         (%plist-when
          :type (%enum-type (ag-ui-protocol:ag-ui-event-type event))
          :timestamp (let ((ts (%slot event "TIMESTAMP")))
                       (and ts (truncate ts)))
          :raw-event (%lisp-to-value (%slot event "RAW-EVENT"))
          :metadata (%table-to-struct (%slot event "METADATA")))))

(defun %apply-base (keys pb-event &optional extra)
  (let ((base (%pb-field pb-event "BASE-EVENT")))
    (append
     keys
     (%plist-when
      :timestamp (and base (cl-protobufs:has-field base 'pb:timestamp)
                      (%pb-field base "TIMESTAMP"))
      :raw-event (and base (cl-protobufs:has-field base 'pb:raw-event)
                      (%value-to-lisp (%pb-field base "RAW-EVENT")))
      :metadata (and base (cl-protobufs:has-field base 'pb:metadata)
                     (%struct-to-table (%pb-field base "METADATA")))
      :subagent-run-id (or extra (%sid-from pb-event))))))

(defun %patch-op (string)
  (cond
    ((string-equal string "add") :add)
    ((string-equal string "remove") :remove)
    ((string-equal string "replace") :replace)
    ((string-equal string "move") :move)
    ((string-equal string "copy") :copy)
    ((string-equal string "test") :test)
    (t :add)))

(defun %op-string (keyword)
  (string-downcase (symbol-name keyword)))

(defun %lisp-patch-to-pb (op)
  (apply #'pb:make-json-patch-operation
         (%plist-when
          :op (%patch-op (ag-ui-protocol:param op "op"))
          :path (or (ag-ui-protocol:param op "path") "")
          :from (ag-ui-protocol:param op "from")
          :value (let ((v (ag-ui-protocol:param op "value" :missing)))
                   (unless (eq v :missing)
                     (%lisp-to-value v))))))

(defun %pb-patch-to-lisp (op)
  (ag-ui-protocol:json-object
   "op" (%op-string (pb:json-patch-operation.op op))
   "path" (pb:json-patch-operation.path op)
   "from" (and (cl-protobufs:has-field op 'pb:from)
               (pb:json-patch-operation.from op))
   "value" (and (cl-protobufs:has-field op 'pb:value)
                (%value-to-lisp (pb:json-patch-operation.value op)))))

(defun %lisp-tool-call-to-pb (table)
  (let ((fn (ag-ui-protocol:param table "function")))
    (apply #'pb:make-tool-call
           (%plist-when
            :id (or (ag-ui-protocol:param table "id") "")
            :type (or (ag-ui-protocol:param table "type") "function")
            :function (pb:make-tool-call.function
                       :name (or (ag-ui-protocol:param fn "name") "")
                       :arguments (or (ag-ui-protocol:param fn "arguments") ""))
            :metadata (%table-to-struct
                       (ag-ui-protocol:param table "metadata"))))))

(defun %pb-tool-call-to-lisp (tc)
  (let ((fn (pb:tool-call.function tc)))
    (ag-ui-protocol:json-object
     "id" (pb:tool-call.id tc)
     "type" (pb:tool-call.type tc)
     "function" (ag-ui-protocol:json-object
                 "name" (and fn (pb:tool-call.function.name fn))
                 "arguments" (and fn (pb:tool-call.function.arguments fn)))
     "metadata" (and (cl-protobufs:has-field tc 'pb:metadata)
                     (%struct-to-table (pb:tool-call.metadata tc))))))

(defun %lisp-message-to-pb (msg)
  (let ((content (%slot msg "CONTENT")))
    (apply #'pb:make-message
           (%plist-when
            :id (ag-ui-protocol:ag-ui-message-id msg)
            :role (ag-ui-protocol:ag-ui-message-role msg)
            :content (and (stringp content) content)
            :name (%slot msg "NAME")
            :tool-call-id (%slot msg "TOOL-CALL-ID")
            :error (%slot msg "ERROR")
            :tool-calls (mapcar #'%lisp-tool-call-to-pb
                                (%as-list (%slot msg "TOOL-CALLS")))))))

(defun %pb-message-to-lisp (msg)
  (ag-ui-protocol::%make
   'ag-ui-protocol:ag-ui-message
   :id (pb:message.id msg)
   :role (pb:message.role msg)
   :content (and (cl-protobufs:has-field msg 'pb:content)
                 (pb:message.content msg))
   :name (and (cl-protobufs:has-field msg 'pb:name)
              (pb:message.name msg))
   :tool-call-id (and (cl-protobufs:has-field msg 'pb:tool-call-id)
                      (pb:message.tool-call-id msg))
   :error (and (cl-protobufs:has-field msg 'pb:error)
               (pb:message.error msg))
   :tool-calls (let ((calls (pb:message.tool-calls msg)))
                 (and calls (plusp (length calls))
                      (coerce (mapcar #'%pb-tool-call-to-lisp calls) 'vector)))))

(defun %lisp-interrupt-to-pb (int)
  (apply #'pb:make-interrupt
         (%plist-when
          :id (ag-ui-protocol:interrupt-id int)
          :reason (ag-ui-protocol:interrupt-reason int)
          :message (%slot int "MESSAGE")
          :tool-call-id (%slot int "TOOL-CALL-ID")
          :response-schema (%lisp-to-value (%slot int "RESPONSE-SCHEMA"))
          :expires-at (%slot int "EXPIRES-AT")
          :metadata (%lisp-to-value (%slot int "METADATA"))
          :subagent-run-id (%slot int "SUBAGENT-RUN-ID"))))

(defun %pb-interrupt-to-lisp (int)
  (ag-ui-protocol::%make
   'ag-ui-protocol:interrupt
   :id (pb:interrupt.id int)
   :reason (pb:interrupt.reason int)
   :message (and (cl-protobufs:has-field int 'pb:message)
                 (pb:interrupt.message int))
   :tool-call-id (and (cl-protobufs:has-field int 'pb:tool-call-id)
                      (pb:interrupt.tool-call-id int))
   :response-schema (and (cl-protobufs:has-field int 'pb:response-schema)
                         (%value-to-lisp (pb:interrupt.response-schema int)))
   :expires-at (and (cl-protobufs:has-field int 'pb:expires-at)
                    (pb:interrupt.expires-at int))
   :metadata (and (cl-protobufs:has-field int 'pb:metadata)
                  (let ((v (%value-to-lisp (pb:interrupt.metadata int))))
                    (and (hash-table-p v) v)))
   :subagent-run-id (and (cl-protobufs:has-field int 'pb:subagent-run-id)
                         (pb:interrupt.subagent-run-id int))))

(defun %run-finished-wire (event)
  (let ((outcome (%slot event "OUTCOME")))
    (cond
      ((typep outcome 'ag-ui-protocol:run-interrupt-outcome)
       (values "interrupt"
               (map 'list #'%lisp-interrupt-to-pb
                    (ag-ui-protocol:outcome-interrupts outcome))))
      ((typep outcome 'ag-ui-protocol:run-success-outcome)
       (values "success" nil))
      (t (values "" nil)))))

(defun %run-finished-outcome (wire-outcome interrupts)
  (cond
    ((string= wire-outcome "interrupt")
     (ag-ui-protocol:make-run-interrupt-outcome :interrupts interrupts))
    ((string= wire-outcome "success")
     (ag-ui-protocol:make-run-success-outcome))
    (t nil)))

(defun %subagent-finished-wire (event)
  (let ((outcome (%slot event "OUTCOME")))
    (cond
      ((hash-table-p outcome)
       (let ((type (ag-ui-protocol:param outcome "type")))
         (cond
           ((string= type "suspended")
            (values "suspended"
                    (%as-list
                     (or (ag-ui-protocol:param outcome "interruptIds")
                         (ag-ui-protocol:param outcome "interrupt-ids")))))
           ((string= type "success")
            (values "success" nil))
           (t (values (or type "") nil)))))
      (t (values "" nil)))))

(defun %subagent-finished-outcome (wire-outcome interrupt-ids)
  (cond
    ((string= wire-outcome "suspended")
     (ag-ui-protocol:json-object
      "type" "suspended"
      "interruptIds" (coerce interrupt-ids 'vector)))
    ((string= wire-outcome "success")
     (ag-ui-protocol:json-object "type" "success"))
    (t nil)))

(defun %unsupported (event)
  (error 'ag-ui-protocol:ag-ui-error
         :message (format nil "official Event oneof has no field for ~A"
                          (ag-ui-protocol:ag-ui-event-type event))))

(defun %wrap (field inner)
  (apply #'pb:make-event (list field inner)))

(defun encode-event (event)
  (etypecase event
    (ag-ui-protocol:run-started-event
     (%wrap :run-started
            (pb:make-run-started-event
             :base-event (%base-event event)
             :thread-id (ag-ui-protocol:run-started-thread-id event)
             :run-id (ag-ui-protocol:run-started-run-id event))))
    (ag-ui-protocol:run-finished-event
     (multiple-value-bind (outcome interrupts)
         (%run-finished-wire event)
       (apply #'pb:make-event
              (list :run-finished
                    (apply #'pb:make-run-finished-event
                           (append
                            (list :base-event (%base-event event)
                                  :thread-id (ag-ui-protocol:run-finished-thread-id event)
                                  :run-id (ag-ui-protocol:run-finished-run-id event)
                                  :outcome outcome
                                  :interrupts interrupts)
                            (%plist-when
                             :result (%lisp-to-value
                                      (%slot event "RESULT")))))))))
    (ag-ui-protocol:run-error-event
     (apply #'pb:make-event
            (list :run-error
                  (apply #'pb:make-run-error-event
                         (append
                          (list :base-event (%base-event event)
                                :message (ag-ui-protocol:run-error-message event))
                          (%plist-when
                           :code (%slot event "CODE")))))))
    (ag-ui-protocol:step-started-event
     (apply #'pb:make-event
            (list :step-started
                  (apply #'pb:make-step-started-event
                         (append
                          (list :base-event (%base-event event)
                                :step-name (ag-ui-protocol:step-event-name event))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:step-finished-event
     (apply #'pb:make-event
            (list :step-finished
                  (apply #'pb:make-step-finished-event
                         (append
                          (list :base-event (%base-event event)
                                :step-name (ag-ui-protocol:step-event-name event))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:text-message-start-event
     (apply #'pb:make-event
            (list :text-message-start
                  (apply #'pb:make-text-message-start-event
                         (append
                          (list :base-event (%base-event event)
                                :message-id (ag-ui-protocol:text-message-id event))
                          (%plist-when
                           :role (%slot event "ROLE")
                           :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:text-message-content-event
     (apply #'pb:make-event
            (list :text-message-content
                  (apply #'pb:make-text-message-content-event
                         (append
                          (list :base-event (%base-event event)
                                :message-id (ag-ui-protocol:text-message-id event)
                                :delta (ag-ui-protocol:text-message-delta event))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:text-message-end-event
     (apply #'pb:make-event
            (list :text-message-end
                  (apply #'pb:make-text-message-end-event
                         (append
                          (list :base-event (%base-event event)
                                :message-id (ag-ui-protocol:text-message-id event))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:text-message-chunk-event
     (apply #'pb:make-event
            (list :text-message-chunk
                  (apply #'pb:make-text-message-chunk-event
                         (%plist-when
                          :base-event (%base-event event)
                          :message-id (%slot event "MESSAGE-ID")
                          :role (%slot event "ROLE")
                          :delta (%slot event "DELTA")
                          :name (%slot event "NAME")
                          :subagent-run-id (%sid event))))))
    (ag-ui-protocol:tool-call-start-event
     (apply #'pb:make-event
            (list :tool-call-start
                  (apply #'pb:make-tool-call-start-event
                         (append
                          (list :base-event (%base-event event)
                                :tool-call-id (ag-ui-protocol:tool-call-id event)
                                :tool-call-name (ag-ui-protocol:tool-call-name event))
                          (%plist-when
                           :parent-message-id (%slot event "PARENT-MESSAGE-ID")
                           :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:tool-call-args-event
     (apply #'pb:make-event
            (list :tool-call-args
                  (apply #'pb:make-tool-call-args-event
                         (append
                          (list :base-event (%base-event event)
                                :tool-call-id (ag-ui-protocol:tool-call-id event)
                                :delta (ag-ui-protocol:tool-call-delta event))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:tool-call-end-event
     (apply #'pb:make-event
            (list :tool-call-end
                  (apply #'pb:make-tool-call-end-event
                         (append
                          (list :base-event (%base-event event)
                                :tool-call-id (ag-ui-protocol:tool-call-id event))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:tool-call-chunk-event
     (apply #'pb:make-event
            (list :tool-call-chunk
                  (apply #'pb:make-tool-call-chunk-event
                         (%plist-when
                          :base-event (%base-event event)
                          :tool-call-id (%slot event "TOOL-CALL-ID")
                          :tool-call-name (%slot event "TOOL-CALL-NAME")
                          :parent-message-id (%slot event "PARENT-MESSAGE-ID")
                          :delta (%slot event "DELTA")
                          :subagent-run-id (%sid event))))))
    (ag-ui-protocol:state-snapshot-event
     (apply #'pb:make-event
            (list :state-snapshot
                  (apply #'pb:make-state-snapshot-event
                         (append
                          (list :base-event (%base-event event)
                                :snapshot (%lisp-to-value
                                           (ag-ui-protocol:state-snapshot-value event)))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:state-delta-event
     (apply #'pb:make-event
            (list :state-delta
                  (apply #'pb:make-state-delta-event
                         (append
                          (list :base-event (%base-event event)
                                :delta (map 'list #'%lisp-patch-to-pb
                                            (ag-ui-protocol:state-delta-patch event)))
                          (%plist-when :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:messages-snapshot-event
     (%wrap :messages-snapshot
            (pb:make-messages-snapshot-event
             :base-event (%base-event event)
             :messages (map 'list #'%lisp-message-to-pb
                            (ag-ui-protocol:messages-snapshot-messages event)))))
    (ag-ui-protocol:raw-event
     (apply #'pb:make-event
            (list :raw
                  (apply #'pb:make-raw-event
                         (append
                          (list :base-event (%base-event event)
                                :event (%lisp-to-value
                                        (ag-ui-protocol:raw-event-payload event)))
                          (%plist-when
                           :source (%slot event "SOURCE")
                           :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:custom-event
     (apply #'pb:make-event
            (list :custom
                  (apply #'pb:make-custom-event
                         (append
                          (list :base-event (%base-event event)
                                :name (ag-ui-protocol:custom-event-name event))
                          (%plist-when
                           :value (%lisp-to-value
                                   (%slot event "VALUE"))
                           :subagent-run-id (%sid event)))))))
    (ag-ui-protocol:subagent-started-event
     (apply #'pb:make-event
            (list :subagent-started
                  (apply #'pb:make-subagent-started-event
                         (append
                          (list :base-event (%base-event event)
                                :subagent-run-id (ag-ui-protocol:ag-ui-event-subagent-run-id event)
                                :name (ag-ui-protocol:subagent-name event))
                          (%plist-when
                           :description (%slot event "DESCRIPTION")
                           :parent-subagent-run-id (%slot event "PARENT-SUBAGENT-RUN-ID")
                           :parent-tool-call-id (%slot event "PARENT-TOOL-CALL-ID")
                           :parent-message-id (%slot event "PARENT-MESSAGE-ID")))))))
    (ag-ui-protocol:subagent-finished-event
     (multiple-value-bind (outcome interrupt-ids)
         (%subagent-finished-wire event)
       (apply #'pb:make-event
              (list :subagent-finished
                    (apply #'pb:make-subagent-finished-event
                           (append
                            (list :base-event (%base-event event)
                                  :subagent-run-id (ag-ui-protocol:ag-ui-event-subagent-run-id event)
                                  :outcome outcome
                                  :interrupt-ids interrupt-ids)
                            (%plist-when
                             :result (%lisp-to-value
                                      (%slot event "RESULT")))))))))
    (ag-ui-protocol:subagent-error-event
     (apply #'pb:make-event
            (list :subagent-error
                  (apply #'pb:make-subagent-error-event
                         (append
                          (list :base-event (%base-event event)
                                :subagent-run-id (ag-ui-protocol:ag-ui-event-subagent-run-id event)
                                :message (ag-ui-protocol:run-error-message event))
                          (%plist-when
                           :code (%slot event "CODE")))))))
    (ag-ui-protocol:ag-ui-event
     (%unsupported event))))

(defun %sid-from (inner)
  (let ((acc (and inner
                  (find-symbol
                   (format nil "~A.SUBAGENT-RUN-ID"
                           (symbol-name (class-name (class-of inner))))
                   :cl-protobufs.ag-ui))))
    (when (and acc (fboundp acc)
               (ignore-errors (cl-protobufs:has-field inner 'pb:subagent-run-id)))
      (let ((s (funcall acc inner)))
        (and (stringp s) (plusp (length s)) s)))))

(defun decode-event (octets)
  (unless (and (vectorp octets) (not (stringp octets)))
    (error 'ag-ui-protocol:ag-ui-error
           :message "oneof decode needs an octet vector"))
  (let* ((msg (protobuf-protocol:decode-octets octets 'pb:event))
         (raw-case (pb:event.event-case msg))
         (case (and raw-case (intern (symbol-name raw-case) :keyword))))
    (unless case
      (error 'ag-ui-protocol:ag-ui-error
             :message "official Event oneof was empty"))
    (let ((inner (ecase case
                   (:text-message-start (pb:event.text-message-start msg))
                   (:text-message-content (pb:event.text-message-content msg))
                   (:text-message-end (pb:event.text-message-end msg))
                   (:text-message-chunk (pb:event.text-message-chunk msg))
                   (:tool-call-start (pb:event.tool-call-start msg))
                   (:tool-call-args (pb:event.tool-call-args msg))
                   (:tool-call-end (pb:event.tool-call-end msg))
                   (:tool-call-chunk (pb:event.tool-call-chunk msg))
                   (:state-snapshot (pb:event.state-snapshot msg))
                   (:state-delta (pb:event.state-delta msg))
                   (:messages-snapshot (pb:event.messages-snapshot msg))
                   (:raw (pb:event.raw msg))
                   (:custom (pb:event.custom msg))
                   (:run-started (pb:event.run-started msg))
                   (:run-finished (pb:event.run-finished msg))
                   (:run-error (pb:event.run-error msg))
                   (:step-started (pb:event.step-started msg))
                   (:step-finished (pb:event.step-finished msg))
                   (:subagent-started (pb:event.subagent-started msg))
                   (:subagent-finished (pb:event.subagent-finished msg))
                   (:subagent-error (pb:event.subagent-error msg)))))
      (ecase case
        (:run-started
         (apply #'ag-ui-protocol:make-run-started-event
                (%apply-base
                 (list :thread-id (pb:run-started-event.thread-id inner)
                       :run-id (pb:run-started-event.run-id inner))
                 inner)))
        (:run-finished
         (apply #'ag-ui-protocol:make-run-finished-event
                (%apply-base
                 (list :thread-id (pb:run-finished-event.thread-id inner)
                       :run-id (pb:run-finished-event.run-id inner)
                       :result (and (cl-protobufs:has-field inner 'pb:result)
                                    (%value-to-lisp
                                     (pb:run-finished-event.result inner)))
                       :outcome (%run-finished-outcome
                                 (pb:run-finished-event.outcome inner)
                                 (mapcar #'%pb-interrupt-to-lisp
                                         (or (pb:run-finished-event.interrupts inner)
                                             '()))))
                 inner)))
        (:run-error
         (apply #'ag-ui-protocol:make-run-error-event
                (%apply-base
                 (list :message (pb:run-error-event.message inner)
                       :code (and (cl-protobufs:has-field inner 'pb:code)
                                  (pb:run-error-event.code inner)))
                 inner)))
        (:step-started
         (apply #'ag-ui-protocol:make-step-started-event
                (%apply-base
                 (list :step-name (pb:step-started-event.step-name inner))
                 inner (%sid-from inner))))
        (:step-finished
         (apply #'ag-ui-protocol:make-step-finished-event
                (%apply-base
                 (list :step-name (pb:step-finished-event.step-name inner))
                 inner (%sid-from inner))))
        (:text-message-start
         (apply #'ag-ui-protocol:make-text-message-start-event
                (%apply-base
                 (list :message-id (pb:text-message-start-event.message-id inner)
                       :role (or (and (cl-protobufs:has-field inner 'pb:role)
                                      (pb:text-message-start-event.role inner))
                                 "assistant"))
                 inner (%sid-from inner))))
        (:text-message-content
         (apply #'ag-ui-protocol:make-text-message-content-event
                (%apply-base
                 (list :message-id (pb:text-message-content-event.message-id inner)
                       :delta (pb:text-message-content-event.delta inner))
                 inner (%sid-from inner))))
        (:text-message-end
         (apply #'ag-ui-protocol:make-text-message-end-event
                (%apply-base
                 (list :message-id (pb:text-message-end-event.message-id inner))
                 inner (%sid-from inner))))
        (:text-message-chunk
         (apply #'ag-ui-protocol:make-text-message-chunk-event
                (%apply-base
                 (%plist-when
                  :message-id (and (cl-protobufs:has-field inner 'pb:message-id)
                                   (pb:text-message-chunk-event.message-id inner))
                  :role (and (cl-protobufs:has-field inner 'pb:role)
                             (pb:text-message-chunk-event.role inner))
                  :delta (and (cl-protobufs:has-field inner 'pb:delta)
                              (pb:text-message-chunk-event.delta inner))
                  :name (and (cl-protobufs:has-field inner 'pb:name)
                             (pb:text-message-chunk-event.name inner)))
                 inner (%sid-from inner))))
        (:tool-call-start
         (apply #'ag-ui-protocol:make-tool-call-start-event
                (%apply-base
                 (list :tool-call-id (pb:tool-call-start-event.tool-call-id inner)
                       :tool-call-name (pb:tool-call-start-event.tool-call-name inner)
                       :parent-message-id
                       (and (cl-protobufs:has-field inner 'pb:parent-message-id)
                            (pb:tool-call-start-event.parent-message-id inner)))
                 inner (%sid-from inner))))
        (:tool-call-args
         (apply #'ag-ui-protocol:make-tool-call-args-event
                (%apply-base
                 (list :tool-call-id (pb:tool-call-args-event.tool-call-id inner)
                       :delta (pb:tool-call-args-event.delta inner))
                 inner (%sid-from inner))))
        (:tool-call-end
         (apply #'ag-ui-protocol:make-tool-call-end-event
                (%apply-base
                 (list :tool-call-id (pb:tool-call-end-event.tool-call-id inner))
                 inner (%sid-from inner))))
        (:tool-call-chunk
         (apply #'ag-ui-protocol:make-tool-call-chunk-event
                (%apply-base
                 (%plist-when
                  :tool-call-id (and (cl-protobufs:has-field inner 'pb:tool-call-id)
                                     (pb:tool-call-chunk-event.tool-call-id inner))
                  :tool-call-name (and (cl-protobufs:has-field inner 'pb:tool-call-name)
                                       (pb:tool-call-chunk-event.tool-call-name inner))
                  :parent-message-id (and (cl-protobufs:has-field inner 'pb:parent-message-id)
                                          (pb:tool-call-chunk-event.parent-message-id inner))
                  :delta (and (cl-protobufs:has-field inner 'pb:delta)
                              (pb:tool-call-chunk-event.delta inner)))
                 inner (%sid-from inner))))
        (:state-snapshot
         (apply #'ag-ui-protocol:make-state-snapshot-event
                (%apply-base
                 (list :snapshot (%value-to-lisp
                                  (pb:state-snapshot-event.snapshot inner)))
                 inner (%sid-from inner))))
        (:state-delta
         (apply #'ag-ui-protocol:make-state-delta-event
                (%apply-base
                 (list :delta (mapcar #'%pb-patch-to-lisp
                                      (or (pb:state-delta-event.delta inner) '())))
                 inner (%sid-from inner))))
        (:messages-snapshot
         (apply #'ag-ui-protocol:make-messages-snapshot-event
                (%apply-base
                 (list :messages (mapcar #'%pb-message-to-lisp
                                         (or (pb:messages-snapshot-event.messages inner)
                                             '())))
                 inner)))
        (:raw
         (apply #'ag-ui-protocol:make-raw-event
                (%apply-base
                 (list :event (%value-to-lisp (pb:raw-event.event inner))
                       :source (and (cl-protobufs:has-field inner 'pb:source)
                                    (pb:raw-event.source inner)))
                 inner (%sid-from inner))))
        (:custom
         (apply #'ag-ui-protocol:make-custom-event
                (%apply-base
                 (list :name (pb:custom-event.name inner)
                       :value (and (cl-protobufs:has-field inner 'pb:value)
                                   (%value-to-lisp (pb:custom-event.value inner))))
                 inner (%sid-from inner))))
        (:subagent-started
         (apply #'ag-ui-protocol:make-subagent-started-event
                (%apply-base
                 (list :subagent-run-id (pb:subagent-started-event.subagent-run-id inner)
                       :name (pb:subagent-started-event.name inner)
                       :description (and (cl-protobufs:has-field inner 'pb:description)
                                         (pb:subagent-started-event.description inner))
                       :parent-subagent-run-id
                       (and (cl-protobufs:has-field inner 'pb:parent-subagent-run-id)
                            (pb:subagent-started-event.parent-subagent-run-id inner))
                       :parent-tool-call-id
                       (and (cl-protobufs:has-field inner 'pb:parent-tool-call-id)
                            (pb:subagent-started-event.parent-tool-call-id inner))
                       :parent-message-id
                       (and (cl-protobufs:has-field inner 'pb:parent-message-id)
                            (pb:subagent-started-event.parent-message-id inner)))
                 inner)))
        (:subagent-finished
         (apply #'ag-ui-protocol:make-subagent-finished-event
                (%apply-base
                 (list :subagent-run-id (pb:subagent-finished-event.subagent-run-id inner)
                       :result (and (cl-protobufs:has-field inner 'pb:result)
                                    (%value-to-lisp
                                     (pb:subagent-finished-event.result inner)))
                       :outcome (%subagent-finished-outcome
                                 (pb:subagent-finished-event.outcome inner)
                                 (or (pb:subagent-finished-event.interrupt-ids inner)
                                     '())))
                 inner)))
        (:subagent-error
         (apply #'ag-ui-protocol:make-subagent-error-event
                (%apply-base
                 (list :subagent-run-id (pb:subagent-error-event.subagent-run-id inner)
                       :message (pb:subagent-error-event.message inner)
                       :code (and (cl-protobufs:has-field inner 'pb:code)
                                  (pb:subagent-error-event.code inner)))
                 inner)))))))

(defun %encode-octets (event)
  (protobuf-protocol:encode-to-octets (encode-event event)))

(in-package #:ag-ui-protocol)

(setf *ag-ui-oneof-encoder* #'ag-ui-protocol/oneof::%encode-octets
      *ag-ui-oneof-decoder* #'ag-ui-protocol/oneof::decode-event)
