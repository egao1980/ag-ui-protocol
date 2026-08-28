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

(deftest protobuf-is-json-octets
  (let* ((ev (ag-ui-protocol:make-text-message-content-event
              :message-id "m" :delta "hi"))
         (octets (ag-ui-protocol:encode-ag-ui-event ev :format :protobuf))
         (back (ag-ui-protocol:decode-ag-ui-event octets :format :protobuf)))
    (ok (vectorp octets))
    (ng (stringp octets))
    (ok (equal "hi" (ag-ui-protocol:text-message-delta back)))))

(deftest unknown-type-errors
  (ok (signals (ag-ui-protocol:decode-ag-ui-event
                "{\"type\":\"REASONING_START\",\"messageId\":\"x\"}")
               'ag-ui-protocol:ag-ui-error)))

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
    (ok (= 404 (first (funcall app (list :request-method :get :path-info "/run")))))
    (ok (= 404 (first (funcall app (list :request-method :post :path-info "/")))))))

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
  (ok (signals (ag-ui-protocol:validate-ag-ui-json
                "{\"type\":\"REASONING_START\",\"messageId\":\"x\"}")
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

