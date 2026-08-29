(defpackage #:ag-ui-client
  (:use #:cl)
  (:local-nicknames (#:ag-ui #:ag-ui-protocol)
                    (#:patch #:json-patch))
  (:documentation
   "Client-side runtime for AG-UI event streams: ordering verification and the
    reducer that folds a stream into messages and state.

    The counterpart of @ag-ui/client's verifyEvents and defaultApplyEvents. A
    UI, a TUI, a test harness, and a proxy all need the same two things, so
    they live here rather than being re-derived per consumer.")
  (:export
   ;; verification
   #:ag-ui-verify-error
   #:ag-ui-verify-error-event
   #:stream-verifier
   #:make-stream-verifier
   #:verify-event
   #:finish-verify
   #:verify-events
   ;; reduction
   #:agent-state
   #:make-agent-state
   #:agent-state-messages
   #:agent-state-value
   #:agent-state-status
   #:agent-state-error-message
   #:agent-state-active-steps
   #:agent-state-interrupts
   #:apply-event
   #:apply-events
   #:reduce-events
   #:message-text))

(in-package #:ag-ui-client)
