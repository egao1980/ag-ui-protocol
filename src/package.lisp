(defpackage #:ag-ui-protocol
  (:use #:cl)
  (:nicknames #:stack-ag-ui)
  (:export #:ag-ui-error
           #:ag-ui-error-message
           #:ag-ui-event
           #:ag-ui-event-type
           #:run-agent-input
           #:ag-ui-backend
           #:*ag-ui-backend*
           #:run-agent
           #:encode-ag-ui-event
           #:decode-ag-ui-event
           #:serve-ag-ui))

(in-package #:ag-ui-protocol)
