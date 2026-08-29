(in-package #:ag-ui-protocol/tests)

(defun %run (&rest events)
  (append (list (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r"))
          events
          (list (ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r"))))

(defun %text-message (id text)
  (list (ag-ui-protocol:make-text-message-start-event :message-id id)
        (ag-ui-protocol:make-text-message-content-event :message-id id :delta text)
        (ag-ui-protocol:make-text-message-end-event :message-id id)))

;;; Verification

(deftest verify-accepts-a-well-formed-run
  (ok (ag-ui-client:verify-events (apply #'%run (%text-message "m1" "hi")))))

(deftest verify-requires-run-started-first
  (ok (signals (ag-ui-client:verify-events
                (list (ag-ui-protocol:make-text-message-start-event :message-id "m")))
               'ag-ui-client:ag-ui-verify-error)))

(deftest verify-rejects-events-after-termination
  (ok (signals (ag-ui-client:verify-events
                (append (%run)
                        (list (ag-ui-protocol:make-text-message-start-event
                               :message-id "m"))))
               'ag-ui-client:ag-ui-verify-error))
  ;; RUN_ERROR is terminal too.
  (ok (signals (ag-ui-client:verify-events
                (list (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r")
                      (ag-ui-protocol:make-run-error-event :message "boom")
                      (ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r")))
               'ag-ui-client:ag-ui-verify-error)))

(deftest verify-rejects-unmatched-and-interleaved-messages
  ;; Content with no open message.
  (ok (signals (ag-ui-client:verify-events
                (%run (ag-ui-protocol:make-text-message-content-event
                       :message-id "m" :delta "x")))
               'ag-ui-client:ag-ui-verify-error))
  ;; Two messages open at once.
  (ok (signals (ag-ui-client:verify-events
                (%run (ag-ui-protocol:make-text-message-start-event :message-id "a")
                      (ag-ui-protocol:make-text-message-start-event :message-id "b")))
               'ag-ui-client:ag-ui-verify-error))
  ;; END for a different id than the one open.
  (ok (signals (ag-ui-client:verify-events
                (%run (ag-ui-protocol:make-text-message-start-event :message-id "a")
                      (ag-ui-protocol:make-text-message-end-event :message-id "b")))
               'ag-ui-client:ag-ui-verify-error)))

(deftest verify-rejects-finishing-with-work-open
  (ok (signals (ag-ui-client:verify-events
                (list (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r")
                      (ag-ui-protocol:make-text-message-start-event :message-id "m")
                      (ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r")))
               'ag-ui-client:ag-ui-verify-error))
  (ok (signals (ag-ui-client:verify-events
                (list (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r")
                      (ag-ui-protocol:make-step-started-event :step-name "s")
                      (ag-ui-protocol:make-run-finished-event :thread-id "t" :run-id "r")))
               'ag-ui-client:ag-ui-verify-error)))

(deftest verify-tracks-steps-and-subagents
  (ok (ag-ui-client:verify-events
       (%run (ag-ui-protocol:make-step-started-event :step-name "plan")
             (ag-ui-protocol:make-step-finished-event :step-name "plan"))))
  (ok (signals (ag-ui-client:verify-events
                (%run (ag-ui-protocol:make-step-finished-event :step-name "never")))
               'ag-ui-client:ag-ui-verify-error))
  (ok (ag-ui-client:verify-events
       (%run (ag-ui-protocol:make-subagent-started-event
              :subagent-run-id "s1" :name "researcher")
             (ag-ui-protocol:make-subagent-finished-event :subagent-run-id "s1"))))
  (ok (signals (ag-ui-client:verify-events
                (%run (ag-ui-protocol:make-subagent-finished-event
                       :subagent-run-id "ghost")))
               'ag-ui-client:ag-ui-verify-error)))

(deftest verify-requires-a-terminal-event
  (ok (signals (ag-ui-client:verify-events
                (list (ag-ui-protocol:make-run-started-event
                       :thread-id "t" :run-id "r")))
               'ag-ui-client:ag-ui-verify-error))
  ;; ...unless the caller is verifying a partial stream.
  (ok (ag-ui-client:verify-events
       (list (ag-ui-protocol:make-run-started-event :thread-id "t" :run-id "r"))
       :complete nil)))

(deftest verify-tolerates-unknown-events
  ;; Forward compatibility has to survive the verifier too, or wave 1 is undone.
  (ok (ag-ui-client:verify-events
       (%run (ag-ui-protocol:decode-ag-ui-event "{\"type\":\"NOT_A_REAL_EVENT\"}")))))

;;; Reduction

(deftest reduce-builds-messages
  (let* ((state (ag-ui-client:reduce-events
                 (apply #'%run (%text-message "m1" "hello"))))
         (messages (ag-ui-client:agent-state-messages state)))
    (ok (eq :finished (ag-ui-client:agent-state-status state)))
    (ok (= 1 (length messages)))
    (ok (equal "assistant" (ag-ui-protocol:ag-ui-message-role (first messages))))
    (ok (equal "hello" (ag-ui-client:message-text (first messages))))))

(deftest reduce-concatenates-deltas
  (let ((state (ag-ui-client:reduce-events
                (%run (ag-ui-protocol:make-text-message-start-event :message-id "m")
                      (ag-ui-protocol:make-text-message-content-event
                       :message-id "m" :delta "he")
                      (ag-ui-protocol:make-text-message-content-event
                       :message-id "m" :delta "llo")
                      (ag-ui-protocol:make-text-message-end-event :message-id "m")))))
    (ok (equal "hello" (ag-ui-client:message-text
                        (first (ag-ui-client:agent-state-messages state)))))))

(deftest reduce-expands-chunks
  ;; The reducer should not care which spelling the producer used.
  (let ((state (ag-ui-client:reduce-events
                (%run (ag-ui-protocol:make-text-message-chunk-event
                       :message-id "m" :delta "hi")))))
    (ok (equal "hi" (ag-ui-client:message-text
                     (first (ag-ui-client:agent-state-messages state)))))))

(deftest reduce-builds-tool-calls-and-results
  (let* ((state (ag-ui-client:reduce-events
                 (%run (ag-ui-protocol:make-tool-call-start-event
                        :tool-call-id "c1" :tool-call-name "search")
                       (ag-ui-protocol:make-tool-call-args-event
                        :tool-call-id "c1" :delta "{\"q\"")
                       (ag-ui-protocol:make-tool-call-args-event
                        :tool-call-id "c1" :delta ":1}")
                       (ag-ui-protocol:make-tool-call-end-event :tool-call-id "c1")
                       (ag-ui-protocol:make-tool-call-result-event
                        :message-id "res-1" :tool-call-id "c1" :content "42"))))
         (messages (ag-ui-client:agent-state-messages state))
         (assistant (find "assistant" messages
                          :key #'ag-ui-protocol:ag-ui-message-role :test #'equal))
         (tool (find "tool" messages
                     :key #'ag-ui-protocol:ag-ui-message-role :test #'equal))
         (call (aref (ag-ui-protocol:ag-ui-message-tool-calls assistant) 0)))
    (ok (equal "c1" (gethash "id" call)))
    (ok (equal "search" (gethash "name" (gethash "function" call))))
    (ok (equal "{\"q\":1}" (gethash "arguments" (gethash "function" call))))
    (ok (equal "42" (ag-ui-client:message-text tool)))
    (ok (equal "c1" (ag-ui-protocol:ag-ui-message-tool-call-id tool)))))

(deftest reduce-collects-reasoning-as-its-own-role
  (let* ((state (ag-ui-client:reduce-events
                 (%run (ag-ui-protocol:make-reasoning-start-event :message-id "r1")
                       (ag-ui-protocol:make-reasoning-message-start-event
                        :message-id "r1")
                       (ag-ui-protocol:make-reasoning-message-content-event
                        :message-id "r1" :delta "thinking")
                       (ag-ui-protocol:make-reasoning-message-end-event
                        :message-id "r1")
                       (ag-ui-protocol:make-reasoning-encrypted-value-event
                        :subtype "message" :entity-id "r1" :encrypted-value "blob")
                       (ag-ui-protocol:make-reasoning-end-event :message-id "r1"))))
         (reasoning (find "reasoning" (ag-ui-client:agent-state-messages state)
                          :key #'ag-ui-protocol:ag-ui-message-role :test #'equal)))
    (ok reasoning)
    (ok (equal "thinking" (ag-ui-client:message-text reasoning)))
    (ok (equal "blob" (ag-ui-protocol:ag-ui-message-encrypted-value reasoning)))))

;;; State — the reason wave 4 needed JSON Patch at all

(deftest reduce-applies-state-snapshot-and-delta
  (let ((state (ag-ui-client:reduce-events
                (%run (ag-ui-protocol:make-state-snapshot-event
                       :snapshot (ag-ui-protocol:json-object
                                  "count" 1
                                  "nested" (ag-ui-protocol:json-object "a" "x")))
                      (ag-ui-protocol:make-state-delta-event
                       :delta (list (ag-ui-protocol:json-object
                                     "op" "replace" "path" "/count" "value" 2)
                                    (ag-ui-protocol:json-object
                                     "op" "add" "path" "/nested/b" "value" "y")))))))
    (ok (eql 2 (gethash "count" (ag-ui-client:agent-state-value state))))
    (ok (equal "y" (gethash "b" (gethash "nested"
                                         (ag-ui-client:agent-state-value state)))))
    (ok (equal "x" (gethash "a" (gethash "nested"
                                         (ag-ui-client:agent-state-value state)))))))

(deftest state-snapshot-replaces-rather-than-merges
  (let ((state (ag-ui-client:reduce-events
                (%run (ag-ui-protocol:make-state-snapshot-event
                       :snapshot (ag-ui-protocol:json-object "a" 1 "b" 2))
                      (ag-ui-protocol:make-state-snapshot-event
                       :snapshot (ag-ui-protocol:json-object "a" 9))))))
    (ok (eql 9 (gethash "a" (ag-ui-client:agent-state-value state))))
    (ng (nth-value 1 (gethash "b" (ag-ui-client:agent-state-value state))))))

(deftest messages-snapshot-keeps-untouched-client-side-roles
  ;; A backend that tracks no reasoning omits it, and the client must not lose
  ;; the reasoning it already has.
  (let* ((state (ag-ui-client:reduce-events
                 (%run (ag-ui-protocol:make-reasoning-message-start-event
                        :message-id "r1")
                       (ag-ui-protocol:make-reasoning-message-content-event
                        :message-id "r1" :delta "why")
                       (ag-ui-protocol:make-reasoning-message-end-event
                        :message-id "r1")
                       (ag-ui-protocol:make-messages-snapshot-event
                        :messages (list (ag-ui-protocol:make-ag-ui-message
                                         :id "u1" :role "user" :content "hi"))))))
         (roles (mapcar #'ag-ui-protocol:ag-ui-message-role
                        (ag-ui-client:agent-state-messages state))))
    (ok (member "user" roles :test #'equal))
    (ok (member "reasoning" roles :test #'equal)))
  ;; But a snapshot that does carry reasoning is authoritative for that role.
  (let* ((state (ag-ui-client:reduce-events
                 (%run (ag-ui-protocol:make-reasoning-message-start-event
                        :message-id "r1")
                       (ag-ui-protocol:make-reasoning-message-content-event
                        :message-id "r1" :delta "old")
                       (ag-ui-protocol:make-reasoning-message-end-event
                        :message-id "r1")
                       (ag-ui-protocol:make-messages-snapshot-event
                        :messages (list (ag-ui-protocol:make-ag-ui-message
                                         :id "r2" :role "reasoning"
                                         :content "new"))))))
         (messages (ag-ui-client:agent-state-messages state)))
    (ok (= 1 (length messages)))
    (ok (equal "new" (ag-ui-client:message-text (first messages))))))

(deftest reduce-tracks-activity
  (let* ((state (ag-ui-client:reduce-events
                 (%run (ag-ui-protocol:make-activity-snapshot-event
                        :message-id "a1" :activity-type "PLAN"
                        :content (ag-ui-protocol:json-object "step" 1))
                       (ag-ui-protocol:make-activity-delta-event
                        :message-id "a1" :activity-type "PLAN"
                        :patch (list (ag-ui-protocol:json-object
                                      "op" "replace" "path" "/step" "value" 2))))))
         (activity (find "activity" (ag-ui-client:agent-state-messages state)
                         :key #'ag-ui-protocol:ag-ui-message-role :test #'equal)))
    (ok activity)
    (ok (equal "PLAN" (ag-ui-protocol:ag-ui-message-activity-type activity)))
    (ok (eql 2 (gethash "step" (ag-ui-protocol:ag-ui-message-content activity))))))

(deftest reduce-surfaces-run-error-and-interrupts
  (let ((errored (ag-ui-client:reduce-events
                  (list (ag-ui-protocol:make-run-started-event
                         :thread-id "t" :run-id "r")
                        (ag-ui-protocol:make-run-error-event :message "boom")))))
    (ok (eq :error (ag-ui-client:agent-state-status errored)))
    (ok (equal "boom" (ag-ui-client:agent-state-error-message errored))))
  (let ((paused (ag-ui-client:reduce-events
                 (list (ag-ui-protocol:make-run-started-event
                        :thread-id "t" :run-id "r")
                       (ag-ui-protocol:make-run-interrupted-event
                        :thread-id "t" :run-id "r"
                        :interrupts (list (ag-ui-protocol:make-interrupt
                                           :id "int-1" :reason "tool_call"
                                           :tool-call-id "c1")))))))
    (ok (eq :interrupted (ag-ui-client:agent-state-status paused)))
    (ok (= 1 (length (ag-ui-client:agent-state-interrupts paused))))
    (ok (equal "int-1" (ag-ui-protocol:interrupt-id
                        (first (ag-ui-client:agent-state-interrupts paused)))))))

(deftest reduced-messages-feed-back-as-input
  ;; The round trip that makes the reducer worth having: fold a run, hand the
  ;; messages straight back as the next turn's input.
  (let* ((state (ag-ui-client:reduce-events
                 (apply #'%run (%text-message "m1" "hello"))))
         (input (ag-ui-protocol:make-run-agent-input
                 :thread-id "t" :run-id "r2"
                 :messages (append (ag-ui-client:agent-state-messages state)
                                   (list (ag-ui-protocol:make-ag-ui-message
                                          :id "u2" :role "user" :content "more")))))
         (back (ag-ui-protocol:decode-run-agent-input
                (ag-ui-protocol:encode-json
                 (ag-ui-protocol:encode-run-agent-input input)))))
    (ok (= 2 (length (ag-ui-protocol:run-agent-input-messages back))))
    (ok (equal "hello" (ag-ui-protocol:ag-ui-message-content
                        (aref (ag-ui-protocol:run-agent-input-messages back) 0))))
    (ok (equal "more" (ag-ui-protocol:ag-ui-message-content
                       (aref (ag-ui-protocol:run-agent-input-messages back) 1))))))
