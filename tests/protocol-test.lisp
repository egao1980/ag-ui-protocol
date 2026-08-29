(in-package #:ag-ui-protocol/tests)

;;; Fixtures re-express the public AG-UI event field names (MIT).
;;; Not copied from @ag-ui/core sources.

(defun %roundtrip (event)
  (ag-ui-protocol:decode-ag-ui-event
   (ag-ui-protocol:encode-ag-ui-event event :format :json)))

(deftest classes-exist
  (ok (find-class 'ag-ui-protocol:ag-ui-event))
  (ok (find-class 'ag-ui-protocol:run-agent-input))
  (ok (find-class 'ag-ui-protocol:ag-ui-agent))
  (ok (find-class 'ag-ui-protocol:run-started-event))
  (ok (find-class 'ag-ui-protocol:text-message-content-event))
  (ok (find-class 'ag-ui-protocol:tool-call-result-event))
  (ok (find-class 'ag-ui-protocol:state-delta-event)))

(deftest-parametrize event-type-roundtrip
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
     ((ag-ui-protocol:make-tool-call-start-event :tool-call-id "c1"
                                                :tool-call-name "search")
      "TOOL_CALL_START")
     ((ag-ui-protocol:make-tool-call-args-event :tool-call-id "c1" :delta "{\"q\":1}")
      "TOOL_CALL_ARGS")
     ((ag-ui-protocol:make-tool-call-end-event :tool-call-id "c1")
      "TOOL_CALL_END")
     ((ag-ui-protocol:make-tool-call-result-event :message-id "m2"
                                                 :tool-call-id "c1"
                                                 :content "ok")
      "TOOL_CALL_RESULT")
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
      "MESSAGES_SNAPSHOT"))
  (ok (equal type (ag-ui-protocol:ag-ui-event-type event)))
  (ok (equal type (ag-ui-protocol:ag-ui-event-type (%roundtrip event)))))

(deftest official-json-fixtures
  (let ((started (ag-ui-protocol:decode-ag-ui-event
                  "{\"type\":\"RUN_STARTED\",\"threadId\":\"thread_123\",\"runId\":\"run_456\"}"))
        (text (ag-ui-protocol:decode-ag-ui-event
               "{\"type\":\"TEXT_MESSAGE_START\",\"messageId\":\"msg-789\",\"role\":\"assistant\"}"))
        (delta (ag-ui-protocol:decode-ag-ui-event
                "{\"type\":\"STATE_DELTA\",\"delta\":[{\"op\":\"replace\",\"path\":\"/foo\",\"value\":1}]}")))
    (ok (equal "thread_123" (ag-ui-protocol:run-started-thread-id started)))
    (ok (equal "run_456" (ag-ui-protocol:run-started-run-id started)))
    (ok (equal "msg-789" (ag-ui-protocol:text-message-id text)))
    (ok (equal "assistant" (ag-ui-protocol:text-message-role text)))
    (ok (equal "replace"
               (ag-ui-protocol:param (aref (ag-ui-protocol:state-delta-patch delta) 0)
                                     "op")))))

(deftest omit-optional-fields
  (let ((json (ag-ui-protocol:decode-json
               (ag-ui-protocol:encode-ag-ui-event
                (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r")))))
    (ng (nth-value 1 (gethash "parentRunId" json)))
    (ng (nth-value 1 (gethash "timestamp" json)))
    (ok (equal "t" (gethash "threadId" json)))))

(deftest input-roundtrip
  (let* ((input (ag-ui-protocol:make-run-agent-input
                 :thread-id "thread_123" :run-id "run_456"
                 :messages (list (ag-ui-protocol:make-ag-ui-message
                                  :id "msg_1" :role "user"
                                  :content "Hello, how are you?"))
                 :tools (list (ag-ui-protocol:make-ag-ui-tool
                               :name "search" :description "look up"))
                 :context (list (ag-ui-protocol:make-ag-ui-context
                                 :description "tz" :value "UTC"))))
         (decoded (ag-ui-protocol:decode-run-agent-input
                   (ag-ui-protocol:encode-json
                    (ag-ui-protocol:encode-run-agent-input input)))))
    (ok (equal "thread_123" (ag-ui-protocol:run-agent-input-thread-id decoded)))
    (ok (equal "Hello, how are you?"
               (ag-ui-protocol:ag-ui-message-content
                (aref (ag-ui-protocol:run-agent-input-messages decoded) 0))))
    (ok (equal "search"
               (ag-ui-protocol:ag-ui-tool-name
                (aref (ag-ui-protocol:run-agent-input-tools decoded) 0))))
    (ok (equal "UTC"
               (ag-ui-protocol:ag-ui-context-value
                (aref (ag-ui-protocol:run-agent-input-context decoded) 0))))))

(deftest protobuf-is-wkt-value
  (let* ((ev (ag-ui-protocol:make-text-message-content-event
              :message-id "m" :delta "hi"))
         (octets (ag-ui-protocol:encode-ag-ui-event ev :format :protobuf))
         (back (ag-ui-protocol:decode-ag-ui-event octets :format :protobuf)))
    (ok (vectorp octets))
    (ng (stringp octets))
    (ok (equal "hi" (ag-ui-protocol:text-message-delta back)))
    ;; Not a JSON string in an octet bag — WKT Value does not start with #\{.
    (ng (eql (aref octets 0) #.(char-code #\{)))))

(deftest protobuf-framed-roundtrip
  (let* ((events (list (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r")
                       (ag-ui-protocol:make-text-message-content-event
                        :message-id "m" :delta "hi")
                       (ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r")))
         (octets (apply #'concatenate '(vector (unsigned-byte 8))
                        (mapcar #'ag-ui-protocol:encode-ag-ui-framed events)))
         (back (ag-ui-protocol:decode-ag-ui-framed octets)))
    (ok (= 3 (length back)))
    (ok (equal "hi" (ag-ui-protocol:text-message-delta (second back))))))

(deftest protobuf-unknown-event-survives-wkt
  (let* ((ev (ag-ui-protocol:decode-ag-ui-event
              "{\"type\":\"NOT_A_REAL_EVENT\",\"messageId\":\"x\"}"))
         (back (ag-ui-protocol:decode-ag-ui-event
                (ag-ui-protocol:encode-ag-ui-event ev :format :protobuf)
                :format :protobuf)))
    (ok (typep back 'ag-ui-protocol:unknown-ag-ui-event))
    (ok (equal "NOT_A_REAL_EVENT" (ag-ui-protocol:ag-ui-event-type back)))
    (ok (equal "x" (gethash "messageId"
                            (ag-ui-protocol:unknown-ag-ui-event-table back))))))

(deftest unknown-type-is-tolerated
  ;; A producer on a newer spec revision must not break the stream.
  (let ((ev (ag-ui-protocol:decode-ag-ui-event
             "{\"type\":\"NOT_A_REAL_EVENT\",\"messageId\":\"x\"}")))
    (ok (typep ev 'ag-ui-protocol:unknown-ag-ui-event))
    (ok (typep ev 'ag-ui-protocol:ag-ui-event))
    (ok (equal "NOT_A_REAL_EVENT" (ag-ui-protocol:ag-ui-event-type ev)))
    (ng (ag-ui-protocol:known-event-type-p "NOT_A_REAL_EVENT"))
    (ok (ag-ui-protocol:known-event-type-p "RUN_STARTED"))))

(deftest unknown-type-round-trips-verbatim
  (let* ((wire "{\"type\":\"NOT_A_REAL_EVENT\",\"messageId\":\"x\",\"nested\":{\"a\":[1,2]}}")
         (back (ag-ui-protocol:decode-json
                (ag-ui-protocol:encode-ag-ui-event
                 (ag-ui-protocol:decode-ag-ui-event wire)))))
    (ok (equal "NOT_A_REAL_EVENT" (gethash "type" back)))
    (ok (equal "x" (gethash "messageId" back)))
    (ok (equal 1 (aref (gethash "a" (gethash "nested" back)) 0)))))

(deftest unknown-type-strict-still-signals
  (ok (signals (ag-ui-protocol:decode-ag-ui-event
                "{\"type\":\"NOT_A_REAL_EVENT\"}" :strict t)
               'ag-ui-protocol:ag-ui-error)))

(deftest unknown-type-survives-sse-stream
  ;; The decisive case: one unmodelled event must not abort the surrounding run.
  (let ((events (ag-ui-protocol:decode-ag-ui-sse-stream
                 (concatenate
                  'string
                  "data: {\"type\":\"RUN_STARTED\",\"threadId\":\"t\",\"runId\":\"r\"}"
                  (string #\Newline) (string #\Newline)
                  "data: {\"type\":\"NOT_A_REAL_EVENT\",\"x\":1}"
                  (string #\Newline) (string #\Newline)
                  "data: {\"type\":\"RUN_FINISHED\",\"threadId\":\"t\",\"runId\":\"r\"}"
                  (string #\Newline) (string #\Newline)))))
    (ok (= 3 (length events)))
    (ok (equal '("RUN_STARTED" "NOT_A_REAL_EVENT" "RUN_FINISHED")
               (mapcar #'ag-ui-protocol:ag-ui-event-type events)))
    (ok (typep (second events) 'ag-ui-protocol:unknown-ag-ui-event))
    (ok (typep (third events) 'ag-ui-protocol:run-finished-event))))

(deftest-parametrize reasoning-event-roundtrip
    ((event type)
     ((ag-ui-protocol:make-reasoning-start-event :message-id "r1")
      "REASONING_START")
     ((ag-ui-protocol:make-reasoning-message-start-event :message-id "r1")
      "REASONING_MESSAGE_START")
     ((ag-ui-protocol:make-reasoning-message-content-event
       :message-id "r1" :delta "because")
      "REASONING_MESSAGE_CONTENT")
     ((ag-ui-protocol:make-reasoning-message-end-event :message-id "r1")
      "REASONING_MESSAGE_END")
     ((ag-ui-protocol:make-reasoning-message-chunk-event
       :message-id "r1" :delta "because")
      "REASONING_MESSAGE_CHUNK")
     ((ag-ui-protocol:make-reasoning-end-event :message-id "r1")
      "REASONING_END")
     ((ag-ui-protocol:make-reasoning-encrypted-value-event
       :subtype "message" :entity-id "m1" :encrypted-value "opaque")
      "REASONING_ENCRYPTED_VALUE")
     ((ag-ui-protocol:make-thinking-start-event :title "pondering")
      "THINKING_START")
     ((ag-ui-protocol:make-thinking-end-event)
      "THINKING_END")
     ((ag-ui-protocol:make-text-message-chunk-event :message-id "m1" :delta "hi")
      "TEXT_MESSAGE_CHUNK")
     ((ag-ui-protocol:make-tool-call-chunk-event
       :tool-call-id "c1" :tool-call-name "search" :delta "{}")
      "TOOL_CALL_CHUNK"))
  (ok (equal type (ag-ui-protocol:ag-ui-event-type event)))
  (ok (equal type (ag-ui-protocol:ag-ui-event-type (%roundtrip event)))))

(deftest reasoning-fields-survive-the-wire
  (let ((ev (%roundtrip (ag-ui-protocol:make-reasoning-message-content-event
                         :message-id "r1" :delta "step one"))))
    (ok (typep ev 'ag-ui-protocol:reasoning-message-content-event))
    (ok (equal "r1" (ag-ui-protocol:text-message-id ev)))
    (ok (equal "step one" (ag-ui-protocol:text-message-delta ev))))
  (let ((enc (%roundtrip (ag-ui-protocol:make-reasoning-encrypted-value-event
                          :subtype "tool-call" :entity-id "c1"
                          :encrypted-value "zzz"))))
    (ok (equal "tool-call" (ag-ui-protocol:reasoning-encrypted-subtype enc)))
    (ok (equal "c1" (ag-ui-protocol:reasoning-encrypted-entity-id enc)))
    (ok (equal "zzz" (ag-ui-protocol:reasoning-encrypted-value enc)))))

(deftest deprecated-thinking-events-still-decode
  ;; Producers that have not migrated to REASONING_* must not break the stream.
  (let ((start (ag-ui-protocol:decode-ag-ui-event
                "{\"type\":\"THINKING_TEXT_MESSAGE_START\"}"))
        (content (ag-ui-protocol:decode-ag-ui-event
                  "{\"type\":\"THINKING_TEXT_MESSAGE_CONTENT\",\"delta\":\"hm\"}")))
    (ok (typep start 'ag-ui-protocol:thinking-text-message-start-event))
    (ok (typep content 'ag-ui-protocol:thinking-text-message-content-event))
    (ok (equal "hm" (ag-ui-protocol:text-message-delta content)))))

(defun %types (events)
  (mapcar #'ag-ui-protocol:ag-ui-event-type events))

(deftest text-chunks-expand-to-triad
  (let ((out (ag-ui-protocol:expand-ag-ui-chunks
              (list (ag-ui-protocol:make-text-message-chunk-event
                     :message-id "m1" :delta "he")
                    (ag-ui-protocol:make-text-message-chunk-event :delta "llo")))))
    (ok (equal '("TEXT_MESSAGE_START" "TEXT_MESSAGE_CONTENT" "TEXT_MESSAGE_CONTENT"
                 "TEXT_MESSAGE_END")
               (%types out)))
    (ok (equal "assistant" (ag-ui-protocol:text-message-role (first out))))
    (ok (equal "llo" (ag-ui-protocol:text-message-delta (third out))))))

(deftest text-chunks-close-on-id-switch
  (let ((out (ag-ui-protocol:expand-ag-ui-chunks
              (list (ag-ui-protocol:make-text-message-chunk-event
                     :message-id "m1" :delta "a")
                    (ag-ui-protocol:make-text-message-chunk-event
                     :message-id "m2" :delta "b")))))
    (ok (equal '("TEXT_MESSAGE_START" "TEXT_MESSAGE_CONTENT"
                 "TEXT_MESSAGE_END" "TEXT_MESSAGE_START" "TEXT_MESSAGE_CONTENT"
                 "TEXT_MESSAGE_END")
               (%types out)))))

(deftest tool-chunks-expand-to-triad
  (let ((out (ag-ui-protocol:expand-ag-ui-chunks
              (list (ag-ui-protocol:make-tool-call-chunk-event
                     :tool-call-id "c1" :tool-call-name "search" :delta "{\"q\"")
                    (ag-ui-protocol:make-tool-call-chunk-event :delta ":1}")))))
    (ok (equal '("TOOL_CALL_START" "TOOL_CALL_ARGS" "TOOL_CALL_ARGS" "TOOL_CALL_END")
               (%types out)))
    (ok (equal "search" (ag-ui-protocol:tool-call-name (first out))))))

(deftest tool-chunk-without-name-errors
  (ok (signals (ag-ui-protocol:expand-ag-ui-chunks
                (list (ag-ui-protocol:make-tool-call-chunk-event
                       :tool-call-id "c1" :delta "{}")))
               'ag-ui-protocol:ag-ui-error))
  (ok (signals (ag-ui-protocol:expand-ag-ui-chunks
                (list (ag-ui-protocol:make-text-message-chunk-event :delta "x")))
               'ag-ui-protocol:ag-ui-error)))

(deftest reasoning-chunk-closes-on-empty-delta-and-foreign-event
  (let ((out (ag-ui-protocol:expand-ag-ui-chunks
              (list (ag-ui-protocol:make-reasoning-message-chunk-event
                     :message-id "r1" :delta "why")
                    (ag-ui-protocol:make-reasoning-message-chunk-event
                     :message-id "r1" :delta "")))))
    (ok (equal '("REASONING_MESSAGE_START" "REASONING_MESSAGE_CONTENT"
                 "REASONING_MESSAGE_END")
               (%types out))))
  ;; A non-reasoning event implicitly closes an open reasoning message.
  (let ((out (ag-ui-protocol:expand-ag-ui-chunks
              (list (ag-ui-protocol:make-reasoning-message-chunk-event
                     :message-id "r1" :delta "why")
                    (ag-ui-protocol:make-run-finished-event
                     :thread-id "t" :run-id "r")))))
    (ok (equal '("REASONING_MESSAGE_START" "REASONING_MESSAGE_CONTENT"
                 "REASONING_MESSAGE_END" "RUN_FINISHED")
               (%types out)))))

(deftest non-chunk-events-pass-through-unchanged
  (let* ((input (list (ag-ui-protocol:make-run-started-event
                       :thread-id "t" :run-id "r")
                      (ag-ui-protocol:make-text-message-start-event :message-id "m")
                      (ag-ui-protocol:make-text-message-content-event
                       :message-id "m" :delta "hi")
                      (ag-ui-protocol:make-text-message-end-event :message-id "m")
                      (ag-ui-protocol:make-run-finished-event
                       :thread-id "t" :run-id "r")))
         (out (ag-ui-protocol:expand-ag-ui-chunks input)))
    (ok (equal (%types input) (%types out)))))

(deftest subagent-run-id-is-carried
  (let* ((ev (ag-ui-protocol:make-text-message-content-event
              :message-id "m" :delta "hi"))
         (back (progn (setf (ag-ui-protocol:ag-ui-event-subagent-run-id ev) "sub-1")
                      (%roundtrip ev))))
    (ok (equal "sub-1" (ag-ui-protocol:ag-ui-event-subagent-run-id back))))
  ;; Omitted when unset, so the wire is unchanged for producers that ignore it.
  (let ((json (ag-ui-protocol:decode-json
               (ag-ui-protocol:encode-ag-ui-event
                (ag-ui-protocol:make-text-message-end-event :message-id "m")))))
    (ng (nth-value 1 (gethash "subagentRunId" json)))))

(defparameter +upstream-event-types+
  '("TEXT_MESSAGE_START" "TEXT_MESSAGE_CONTENT" "TEXT_MESSAGE_END"
    "TEXT_MESSAGE_CHUNK"
    "TOOL_CALL_START" "TOOL_CALL_ARGS" "TOOL_CALL_END" "TOOL_CALL_CHUNK"
    "TOOL_CALL_RESULT"
    "THINKING_START" "THINKING_END" "THINKING_TEXT_MESSAGE_START"
    "THINKING_TEXT_MESSAGE_CONTENT" "THINKING_TEXT_MESSAGE_END"
    "STATE_SNAPSHOT" "STATE_DELTA" "MESSAGES_SNAPSHOT"
    "ACTIVITY_SNAPSHOT" "ACTIVITY_DELTA"
    "RAW" "CUSTOM"
    "RUN_STARTED" "RUN_FINISHED" "RUN_ERROR" "STEP_STARTED" "STEP_FINISHED"
    "REASONING_START" "REASONING_MESSAGE_START" "REASONING_MESSAGE_CONTENT"
    "REASONING_MESSAGE_END" "REASONING_MESSAGE_CHUNK" "REASONING_END"
    "REASONING_ENCRYPTED_VALUE"
    "SUBAGENT_STARTED" "SUBAGENT_FINISHED" "SUBAGENT_ERROR")
  "The upstream EventType enum. Field names are public protocol (MIT); this is a
   re-expression, not a copy of @ag-ui/core sources.")

(deftest every-upstream-event-type-is-modelled
  (let ((missing (remove-if #'ag-ui-protocol:known-event-type-p
                            +upstream-event-types+)))
    (ok (null missing)
        (format nil "unmodelled upstream event types: ~{~a~^ ~}" missing))
    (ok (= 36 (length +upstream-event-types+)))))

(deftest activity-and-subagent-events
  (let ((snap (%roundtrip (ag-ui-protocol:make-activity-snapshot-event
                           :message-id "a1" :activity-type "PLAN"
                           :content (ag-ui-protocol:json-object "step" 1))))
        (delta (%roundtrip (ag-ui-protocol:make-activity-delta-event
                            :message-id "a1" :activity-type "PLAN"
                            :patch (list (ag-ui-protocol:json-object
                                          "op" "replace" "path" "/step"
                                          "value" 2)))))
        (started (%roundtrip (ag-ui-protocol:make-subagent-started-event
                              :subagent-run-id "s1" :name "researcher")))
        (err (%roundtrip (ag-ui-protocol:make-subagent-error-event
                          :subagent-run-id "s1" :message "boom"))))
    (ok (equal "PLAN" (ag-ui-protocol:activity-type snap)))
    (ok (equal 1 (gethash "step" (ag-ui-protocol:activity-content snap))))
    (ok (equal "replace" (ag-ui-protocol:param
                          (aref (ag-ui-protocol:activity-patch delta) 0) "op")))
    (ok (equal "researcher" (ag-ui-protocol:subagent-name started)))
    (ok (equal "s1" (ag-ui-protocol:ag-ui-event-subagent-run-id started)))
    (ok (equal "boom" (ag-ui-protocol:run-error-message err)))))

;;; Interrupts

(defun %approval-interrupt (&key (id "int-1") (tool-call-id "tc-1") expires-at)
  (ag-ui-protocol:make-interrupt
   :id id :reason "tool_call" :tool-call-id tool-call-id
   :message "Send email?" :expires-at expires-at
   :response-schema (ag-ui-protocol:json-object
                     "type" "object"
                     "required" (vector "approved")
                     "properties" (ag-ui-protocol:json-object
                                   "approved" (ag-ui-protocol:json-object
                                               "type" "boolean")))))

(deftest run-finished-outcome-roundtrip
  (let* ((ev (ag-ui-protocol:make-run-interrupted-event
              :thread-id "t" :run-id "r"
              :interrupts (list (%approval-interrupt))))
         (back (%roundtrip ev))
         (open (ag-ui-protocol:open-interrupts back)))
    (ok (typep back 'ag-ui-protocol:run-finished-event))
    (ok (ag-ui-protocol:run-interrupted-p back))
    (ok (= 1 (length open)))
    (ok (equal "int-1" (ag-ui-protocol:interrupt-id (first open))))
    (ok (equal "tool_call" (ag-ui-protocol:interrupt-reason (first open))))
    (ok (equal "tc-1" (ag-ui-protocol:interrupt-tool-call-id (first open))))))

(deftest run-finished-without-outcome-is-a-plain-completion
  ;; A producer written before interrupts omits `outcome` entirely.
  (let ((back (ag-ui-protocol:decode-ag-ui-event
               "{\"type\":\"RUN_FINISHED\",\"threadId\":\"t\",\"runId\":\"r\"}")))
    (ng (ag-ui-protocol:run-interrupted-p back))
    (ng (ag-ui-protocol:open-interrupts back)))
  ;; ...and `outcome` is omitted from the wire when unset.
  (let ((json (ag-ui-protocol:decode-json
               (ag-ui-protocol:encode-ag-ui-event
                (ag-ui-protocol:make-run-finished-event
                 :thread-id "t" :run-id "r")))))
    (ng (nth-value 1 (gethash "outcome" json)))))

(deftest success-outcome-roundtrip
  (let ((back (%roundtrip (ag-ui-protocol:make-run-finished-event
                           :thread-id "t" :run-id "r"
                           :outcome (ag-ui-protocol:make-run-success-outcome)))))
    (ok (typep (ag-ui-protocol:run-finished-outcome back)
               'ag-ui-protocol:run-success-outcome))
    (ng (ag-ui-protocol:run-interrupted-p back))))

(deftest resume-input-roundtrip
  (let* ((input (ag-ui-protocol:make-run-agent-input
                 :thread-id "t" :run-id "r2"
                 :resume (list (ag-ui-protocol:make-resume-entry
                                :interrupt-id "int-1"
                                :payload (ag-ui-protocol:json-object
                                          "approved" t)))))
         (back (ag-ui-protocol:decode-run-agent-input
                (ag-ui-protocol:encode-json
                 (ag-ui-protocol:encode-run-agent-input input))))
         (entry (aref (ag-ui-protocol:run-agent-input-resume back) 0)))
    (ok (equal "int-1" (ag-ui-protocol:resume-interrupt-id entry)))
    (ok (equal "resolved" (ag-ui-protocol:resume-status entry)))
    (ok (ag-ui-protocol:resume-approved-p entry))))

(deftest validate-resume-accepts-a-conforming-answer
  (ok (ag-ui-protocol:validate-resume
       (list (%approval-interrupt))
       (list (ag-ui-protocol:make-resume-entry
              :interrupt-id "int-1"
              :payload (ag-ui-protocol:json-object "approved" t))))))

(deftest validate-resume-enforces-the-contract
  (let ((interrupts (list (%approval-interrupt :id "a")
                          (%approval-interrupt :id "b" :tool-call-id "tc-2"))))
    ;; Partial resumes are not supported.
    (ok (signals (ag-ui-protocol:validate-resume
                  interrupts
                  (list (ag-ui-protocol:make-resume-entry
                         :interrupt-id "a"
                         :payload (ag-ui-protocol:json-object "approved" t))))
                 'ag-ui-protocol:ag-ui-resume-error))
    ;; An id that was never open.
    (ok (signals (ag-ui-protocol:validate-resume
                  (list (%approval-interrupt :id "a"))
                  (list (ag-ui-protocol:make-resume-entry :interrupt-id "zzz")))
                 'ag-ui-protocol:ag-ui-resume-error)))
  ;; A payload that fails its own responseSchema.
  (ok (signals (ag-ui-protocol:validate-resume
                (list (%approval-interrupt))
                (list (ag-ui-protocol:make-resume-entry
                       :interrupt-id "int-1"
                       :payload (ag-ui-protocol:json-object "approved" "yes"))))
               'ag-ui-protocol:ag-ui-resume-error))
  ;; Cancelled needs no payload, so the schema is not applied.
  (ok (ag-ui-protocol:validate-resume
       (list (%approval-interrupt))
       (list (ag-ui-protocol:make-resume-entry
              :interrupt-id "int-1" :status "cancelled")))))

(deftest validate-resume-rejects-expired-interrupts
  (let ((interrupts (list (%approval-interrupt :expires-at "2020-01-01T00:00:00Z"))))
    (ok (ag-ui-protocol:interrupt-expired-p (first interrupts)))
    (ok (signals (ag-ui-protocol:validate-resume
                  interrupts
                  (list (ag-ui-protocol:make-resume-entry
                         :interrupt-id "int-1"
                         :payload (ag-ui-protocol:json-object "approved" t))))
                 'ag-ui-protocol:ag-ui-resume-error)))
  ;; An unreadable expiry is left for the agent to adjudicate, not refused here.
  (ng (ag-ui-protocol:interrupt-expired-p
       (%approval-interrupt :expires-at "whenever"))))

(deftest parse-iso8601-handles-offsets
  (ok (= (ag-ui-protocol:parse-iso8601 "2026-04-20T17:00:00Z")
         (ag-ui-protocol:parse-iso8601 "2026-04-20T19:00:00+02:00")))
  (ok (= (ag-ui-protocol:parse-iso8601 "2026-04-20T17:00:00Z")
         (ag-ui-protocol:parse-iso8601 "2026-04-20T17:00:00.123Z")))
  (ng (ag-ui-protocol:parse-iso8601 "not-a-time")))

(deftest pending-interrupts-block-new-input
  (let ((interrupts (list (%approval-interrupt))))
    (ok (signals (ag-ui-protocol:validate-resume-input
                  (ag-ui-protocol:make-run-agent-input :thread-id "t" :run-id "r2")
                  interrupts)
                 'ag-ui-protocol:ag-ui-resume-error))
    (ok (ag-ui-protocol:validate-resume-input
         (ag-ui-protocol:make-run-agent-input
          :thread-id "t" :run-id "r2"
          :resume (list (ag-ui-protocol:make-resume-entry
                         :interrupt-id "int-1"
                         :payload (ag-ui-protocol:json-object "approved" t))))
         interrupts)))
  ;; No interrupts open and no resume sent is the ordinary case.
  (ok (ag-ui-protocol:validate-resume-input
       (ag-ui-protocol:make-run-agent-input :thread-id "t" :run-id "r1")
       nil)))

(deftest resume-decision-helpers
  (let ((denied (ag-ui-protocol:make-resume-entry
                 :interrupt-id "i"
                 :payload (ag-ui-protocol:json-object "approved" nil)))
        (cancelled (ag-ui-protocol:make-resume-entry
                    :interrupt-id "i" :status "cancelled"))
        (edited (ag-ui-protocol:make-resume-entry
                 :interrupt-id "i"
                 :payload (ag-ui-protocol:json-object
                           "approved" t
                           "editedArgs" (ag-ui-protocol:json-object
                                         "to" "a@b.com")))))
    (ng (ag-ui-protocol:resume-approved-p denied))
    (ng (ag-ui-protocol:resume-approved-p cancelled))
    (ok (ag-ui-protocol:resume-approved-p edited))
    (ok (equal "a@b.com" (gethash "to" (ag-ui-protocol:resume-edited-args edited))))
    (ng (ag-ui-protocol:resume-edited-args denied))))

(deftest raw-and-custom-events
  (let ((raw (%roundtrip (ag-ui-protocol:make-raw-event
                          :event (ag-ui-protocol:json-object "kind" "foreign")
                          :source "langgraph")))
        (custom (%roundtrip (ag-ui-protocol:make-custom-event
                             :name "thumbs_up" :value 1))))
    (ok (typep raw 'ag-ui-protocol:raw-event))
    (ok (equal "RAW" (ag-ui-protocol:ag-ui-event-type raw)))
    (ok (equal "langgraph" (ag-ui-protocol:raw-event-source raw)))
    (ok (equal "foreign" (gethash "kind" (ag-ui-protocol:raw-event-payload raw))))
    (ok (typep custom 'ag-ui-protocol:custom-event))
    (ok (equal "thumbs_up" (ag-ui-protocol:custom-event-name custom)))
    (ok (equal 1 (ag-ui-protocol:custom-event-value custom)))))

(deftest echo-handler-sequence
  (let* ((agent (ag-ui-protocol:make-ag-ui-agent))
         (seen '())
         (events (ag-ui-protocol:run-agent
                  agent
                  (ag-ui-protocol:make-run-agent-input
                   :thread-id "t1" :run-id "r1"
                   :messages (list (ag-ui-protocol:make-ag-ui-message
                                    :id "u" :role "user" :content "ping")))
                  :on-event (lambda (ev) (push (ag-ui-protocol:ag-ui-event-type ev)
                                               seen)))))
    (ok (= 5 (length events)))
    (ok (equal '("RUN_STARTED" "TEXT_MESSAGE_START" "TEXT_MESSAGE_CONTENT"
                 "TEXT_MESSAGE_END" "RUN_FINISHED")
               (mapcar #'ag-ui-protocol:ag-ui-event-type events)))
    (ok (equal "ping" (ag-ui-protocol:text-message-delta (third events))))
    (ok (equal (reverse '("RUN_STARTED" "TEXT_MESSAGE_START" "TEXT_MESSAGE_CONTENT"
                          "TEXT_MESSAGE_END" "RUN_FINISHED"))
               seen))))

(defun %clack-body-string (body)
  (cond
    ((functionp body)
     (with-output-to-string (s) (funcall body s)))
    ((listp body) (apply #'concatenate 'string body))
    ((stringp body) body)
    (t "")))

(deftest clack-app-post-sse
  (let* ((agent (ag-ui-protocol:make-ag-ui-agent))
         (app (ag-ui-protocol:make-ag-ui-app agent :path "/"))
         (body (ag-ui-protocol:encode-json
                (ag-ui-protocol:encode-run-agent-input
                 (ag-ui-protocol:make-run-agent-input
                  :thread-id "t" :run-id "r"
                  :messages (list (ag-ui-protocol:make-ag-ui-message
                                   :id "u" :role "user" :content "yo"))))))
         (res (funcall app (list :request-method :post
                                 :path-info "/"
                                 :raw-body body)))
         (status (first res))
         (headers (second res))
         (wire (%clack-body-string (third res)))
         (events (ag-ui-protocol:decode-ag-ui-sse-stream wire)))
    (ok (= 200 status))
    (ok (search "text/event-stream" (getf headers :content-type)))
    (ok (functionp (third res)))
    (ok (= 5 (length events)))
    (ok (equal "yo" (ag-ui-protocol:text-message-delta (third events))))))

(deftest ag-ui-emit-incremental-skips-mapc
  (let* ((emitted '())
         (handler (lambda (input)
                    (declare (ignore input))
                    (ag-ui-protocol:ag-ui-emit
                     (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r"))
                    (ag-ui-protocol:ag-ui-emit
                     (ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r"))
                    (list (ag-ui-protocol:make-run-error-event :message "should-not-mapc"))))
         (agent (ag-ui-protocol:make-ag-ui-agent :handler handler))
         (events (ag-ui-protocol:run-agent
                  agent
                  (ag-ui-protocol:make-run-agent-input :thread-id "t" :run-id "r")
                  :on-event (lambda (ev)
                              (push (ag-ui-protocol:ag-ui-event-type ev) emitted)))))
    (ok (= 2 (length events)))
    (ok (equal '("RUN_STARTED" "RUN_FINISHED")
               (mapcar #'ag-ui-protocol:ag-ui-event-type events)))
    (ok (equal '("RUN_FINISHED" "RUN_STARTED") emitted))))

(deftest clack-app-404
  (let ((app (ag-ui-protocol:make-ag-ui-app (ag-ui-protocol:make-ag-ui-agent)
                                           :path "/run")))
    (ok (= 404 (first (funcall app (list :request-method :post :path-info "/")))))))

(deftest clack-get-returns-capabilities
  (let* ((app (ag-ui-protocol:make-ag-ui-app
               (ag-ui-protocol:make-ag-ui-agent :name "desk")
               :path "/run"))
         (res (funcall app (list :request-method :get :path-info "/run")))
         (caps (ag-ui-protocol:decode-agent-capabilities (first (third res)))))
    (ok (= 200 (first res)))
    (ok (equal "desk" (ag-ui-protocol:identity-name
                       (ag-ui-protocol:capabilities-identity caps))))
    (ok (ag-ui-protocol:transport-streaming-p
         (ag-ui-protocol:capabilities-transport caps)))))

(deftest accept-defaults-to-sse
  (ok (eq :json (ag-ui-protocol:negotiate-ag-ui-format nil)))
  (ok (eq :json (ag-ui-protocol:negotiate-ag-ui-format "*/*")))
  (ok (eq :json (ag-ui-protocol:negotiate-ag-ui-format "text/event-stream")))
  (ok (eq :protobuf
          (ag-ui-protocol:negotiate-ag-ui-format
           "application/vnd.ag-ui.event+proto")))
  (ok (null (ag-ui-protocol:negotiate-ag-ui-format "application/json"))))

(deftest accept-protobuf-roundtrip
  (let* ((app (ag-ui-protocol:make-ag-ui-app (ag-ui-protocol:make-ag-ui-agent)))
         (headers (let ((h (make-hash-table :test 'equal)))
                    (setf (gethash "accept" h) "application/vnd.ag-ui.event+proto")
                    h))
         (body (ag-ui-protocol:encode-json
                (ag-ui-protocol:encode-run-agent-input
                 (ag-ui-protocol:make-run-agent-input
                  :thread-id "t" :run-id "r"
                  :messages (list (ag-ui-protocol:make-ag-ui-message
                                   :role "user" :content "pb"))))))
         (res (funcall app (list :request-method :post
                                 :path-info "/"
                                 :headers headers
                                 :raw-body body)))
         (events (ag-ui-protocol:decode-ag-ui-framed (first (third res)))))
    (ok (= 200 (first res)))
    (ok (search "vnd.ag-ui.event+proto"
                (getf (second res) :content-type)))
    (ok (equal "pb" (ag-ui-protocol:text-message-delta (third events))))))

(deftest sse-encode-has-data-line
  (let ((wire (ag-ui-protocol:encode-ag-ui-sse
               (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r"))))
    (ok (eql 0 (search "data: " wire)))
    (ok (search "\"type\":\"RUN_STARTED\"" wire))))

(deftest json-schema-emit-tagged-events
  (let ((js (ag-ui-protocol:ag-ui-json-schema 'ag-ui-protocol:ag-ui-event)))
    (ok (hash-table-p js))
    (ok (gethash "oneOf" js))
    (ok (equal "type" (gethash "propertyName" (gethash "discriminator" js))))))

(deftest validate-ag-ui-json-official
  (let ((ev (ag-ui-protocol:validate-ag-ui-json
             "{\"type\":\"RUN_STARTED\",\"threadId\":\"thread_123\",\"runId\":\"run_456\"}")))
    (ok (typep ev 'ag-ui-protocol:run-started-event))
    (ok (equal "thread_123" (ag-ui-protocol:run-started-thread-id ev))))
  ;; VALIDATE-AG-UI-JSON stays strict — it is the opt-in checking path, unlike
  ;; DECODE-AG-UI-EVENT which must tolerate unmodelled types off the wire.
  (ok (signals (ag-ui-protocol:validate-ag-ui-json
                "{\"type\":\"NOT_A_REAL_EVENT\",\"messageId\":\"x\"}")
               'ag-ui-protocol:ag-ui-error)))

(deftest validate-tool-arguments-json-schema
  (let* ((params (ag-ui-protocol:json-object
                  "type" "object"
                  "required" (vector "q")
                  "properties" (ag-ui-protocol:json-object
                                "q" (ag-ui-protocol:json-object "type" "string"))
                  "additionalProperties" nil))
         (tool (ag-ui-protocol:make-ag-ui-tool :name "search" :parameters params)))
    (ok (equal "hi" (gethash "q" (ag-ui-protocol:validate-tool-arguments
                                  tool (ag-ui-protocol:json-object "q" "hi")))))
    (ok (signals (ag-ui-protocol:validate-tool-arguments
                  tool (ag-ui-protocol:json-object "q" 1))
                 'ag-ui-protocol:ag-ui-error))))

(deftest run-agent-use-value
  (let ((events (handler-bind ((ag-ui-protocol:ag-ui-error
                                (lambda (c)
                                  (use-value
                                   (list (ag-ui-protocol:make-run-finished-event
                                          :thread-id "t" :run-id "r"))
                                   c))))
                  (ag-ui-protocol:run-agent
                   (make-instance 'ag-ui-protocol:ag-ui-backend)
                   (ag-ui-protocol:make-run-agent-input
                    :thread-id "t" :run-id "r")))))
    (ok (= 1 (length events)))
    (ok (typep (first events) 'ag-ui-protocol:run-finished-event))))

