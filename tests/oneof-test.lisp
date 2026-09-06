(in-package #:ag-ui-protocol/tests)

(defun %oneof (event)
  (ag-ui-protocol:decode-ag-ui-event-oneof
   (ag-ui-protocol:encode-ag-ui-event-oneof event)))

(deftest oneof-supported-types
  (ok (ag-ui-protocol:ag-ui-oneof-event-type-p "RUN_STARTED"))
  (ok (ag-ui-protocol:ag-ui-oneof-event-type-p "TEXT_MESSAGE_CHUNK"))
  (ok (ag-ui-protocol:ag-ui-oneof-event-type-p "SUBAGENT_ERROR"))
  (ng (ag-ui-protocol:ag-ui-oneof-event-type-p "TOOL_CALL_RESULT"))
  (ng (ag-ui-protocol:ag-ui-oneof-event-type-p "REASONING_START"))
  (ng (ag-ui-protocol:ag-ui-oneof-event-type-p "ACTIVITY_SNAPSHOT"))
  (ng (ag-ui-protocol:ag-ui-oneof-event-type-p "THINKING_START"))
  (ok (= 21 (length ag-ui-protocol::+ag-ui-oneof-event-types+)))
  (ok (= 36 (length +upstream-event-types+))))

(deftest-parametrize oneof-roundtrip
    ((event type)
     ((ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r")
      "RUN_STARTED")
     ((ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r")
      "RUN_FINISHED")
     ((ag-ui-protocol:make-run-error-event :message "boom" :code "E")
      "RUN_ERROR")
     ((ag-ui-protocol:make-step-started-event :step-name "think")
      "STEP_STARTED")
     ((ag-ui-protocol:make-step-finished-event :step-name "think")
      "STEP_FINISHED")
     ((ag-ui-protocol:make-text-message-start-event :message-id "m1")
      "TEXT_MESSAGE_START")
     ((ag-ui-protocol:make-text-message-content-event :message-id "m1" :delta "hi")
      "TEXT_MESSAGE_CONTENT")
     ((ag-ui-protocol:make-text-message-end-event :message-id "m1")
      "TEXT_MESSAGE_END")
     ((ag-ui-protocol:make-text-message-chunk-event :message-id "m1" :delta "c")
      "TEXT_MESSAGE_CHUNK")
     ((ag-ui-protocol:make-tool-call-start-event :tool-call-id "c1"
                                                :tool-call-name "echo")
      "TOOL_CALL_START")
     ((ag-ui-protocol:make-tool-call-args-event :tool-call-id "c1" :delta "{}")
      "TOOL_CALL_ARGS")
     ((ag-ui-protocol:make-tool-call-end-event :tool-call-id "c1")
      "TOOL_CALL_END")
     ((ag-ui-protocol:make-tool-call-chunk-event :tool-call-id "c1" :delta "x")
      "TOOL_CALL_CHUNK")
     ((ag-ui-protocol:make-state-snapshot-event
       :snapshot (ag-ui-protocol:json-object "n" 1))
      "STATE_SNAPSHOT")
     ((ag-ui-protocol:make-state-delta-event
       :delta (list (ag-ui-protocol:json-object
                     "op" "replace" "path" "/n" "value" 2)))
      "STATE_DELTA")
     ((ag-ui-protocol:make-messages-snapshot-event
       :messages (list (ag-ui-protocol:make-ag-ui-message
                        :id "m" :role "user" :content "hi")))
      "MESSAGES_SNAPSHOT")
     ((ag-ui-protocol:make-raw-event :event (ag-ui-protocol:json-object "x" 1)
                                    :source "ext")
      "RAW")
     ((ag-ui-protocol:make-custom-event :name "ping" :value "pong")
      "CUSTOM")
     ((ag-ui-protocol:make-subagent-started-event :subagent-run-id "s1"
                                                 :name "researcher")
      "SUBAGENT_STARTED")
     ((ag-ui-protocol:make-subagent-finished-event :subagent-run-id "s1")
      "SUBAGENT_FINISHED")
     ((ag-ui-protocol:make-subagent-error-event :subagent-run-id "s1"
                                               :message "no")
      "SUBAGENT_ERROR"))
  (ok (equal type (ag-ui-protocol:ag-ui-event-type event)))
  (ok (equal type (ag-ui-protocol:ag-ui-event-type (%oneof event)))))

(deftest oneof-unsupported-signals
  (ok (signals (ag-ui-protocol:encode-ag-ui-event-oneof
                (ag-ui-protocol:make-tool-call-result-event
                 :message-id "m" :tool-call-id "c" :content "ok"))
               'ag-ui-protocol:ag-ui-error))
  (ok (signals (ag-ui-protocol:encode-ag-ui-event-oneof
                (ag-ui-protocol:make-reasoning-start-event :message-id "m"))
               'ag-ui-protocol:ag-ui-error))
  (ok (signals (ag-ui-protocol:encode-ag-ui-event-oneof
                (ag-ui-protocol:make-activity-snapshot-event
                 :message-id "a" :activity-type "PLAN"
                 :content (ag-ui-protocol:json-object "n" 1)))
               'ag-ui-protocol:ag-ui-error))
  (ok (signals (ag-ui-protocol:encode-ag-ui-event-oneof
                (ag-ui-protocol:make-thinking-start-event :title "x"))
               'ag-ui-protocol:ag-ui-error)))

(deftest oneof-interrupt-outcome
  (let* ((ev (ag-ui-protocol:make-run-interrupted-event
              :thread-id "t" :run-id "r"
              :interrupts (list (ag-ui-protocol:make-interrupt
                                 :id "int-1" :reason "tool_call"
                                 :message "approve?"))))
         (back (%oneof ev)))
    (ok (equal "interrupt"
               (ag-ui-protocol:run-outcome-type
                (ag-ui-protocol:run-finished-outcome back))))
    (ok (equal "int-1"
               (ag-ui-protocol:interrupt-id
                (aref (ag-ui-protocol:outcome-interrupts
                       (ag-ui-protocol:run-finished-outcome back))
                      0))))))

(deftest oneof-state-delta-op
  (let* ((ev (ag-ui-protocol:make-state-delta-event
              :delta (list (ag-ui-protocol:json-object
                            "op" "replace" "path" "/count" "value" 1))))
         (back (%oneof ev))
         (op (aref (ag-ui-protocol:state-delta-patch back) 0)))
    (ok (equal "replace" (ag-ui-protocol:param op "op")))
    (ok (equal "/count" (ag-ui-protocol:param op "path")))
    (ok (eql 1 (ag-ui-protocol:param op "value")))))

(deftest oneof-framed-stream
  (let* ((events (list (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r")
                       (ag-ui-protocol:make-text-message-content-event
                        :message-id "m" :delta "hi")
                       (ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r")))
         (octets (apply #'concatenate '(vector (unsigned-byte 8))
                        (mapcar #'ag-ui-protocol:encode-ag-ui-framed-oneof events)))
         (back (ag-ui-protocol:decode-ag-ui-framed-oneof octets)))
    (ok (= 3 (length back)))
    (ok (equal "hi" (ag-ui-protocol:text-message-delta (second back))))))

(deftest oneof-not-wkt
  (let* ((ev (ag-ui-protocol:make-text-message-content-event
              :message-id "m" :delta "hi"))
         (oneof (ag-ui-protocol:encode-ag-ui-event-oneof ev))
         (wkt (ag-ui-protocol:encode-ag-ui-event ev :format :protobuf)))
    (ok (vectorp oneof))
    (ng (equalp oneof wkt))
    (ok (equal "hi" (ag-ui-protocol:text-message-delta
                     (ag-ui-protocol:decode-ag-ui-event-oneof oneof))))))

(deftest accept-oneof-roundtrip
  (let* ((app (ag-ui-protocol:make-ag-ui-app (ag-ui-protocol:make-ag-ui-agent)))
         (headers (let ((h (make-hash-table :test 'equal)))
                    (setf (gethash "accept" h) "application/vnd.ag-ui.event+oneof")
                    h))
         (body (ag-ui-protocol:encode-json
                (ag-ui-protocol:encode-run-agent-input
                 (ag-ui-protocol:make-run-agent-input
                  :thread-id "t" :run-id "r"
                  :messages (list (ag-ui-protocol:make-ag-ui-message
                                   :role "user" :content "oo"))))))
         (res (ag-ui-protocol:invoke-ag-ui-app
               app (list :request-method :post
                         :path-info "/"
                         :headers headers
                         :raw-body body)))
         (events (ag-ui-protocol:decode-ag-ui-framed-oneof (first (third res)))))
    (ok (= 200 (first res)))
    (ok (search "vnd.ag-ui.event+oneof"
                (getf (second res) :content-type)))
    (ok (equal "oo" (ag-ui-protocol:text-message-delta (third events))))))
