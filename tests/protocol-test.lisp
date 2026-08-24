(in-package #:ag-ui-protocol/tests)

(deftest classes-exist
  (ok (find-class 'ag-ui-protocol:ag-ui-event))
  (ok (find-class 'ag-ui-protocol:run-agent-input))
  (ok (find-class 'ag-ui-protocol:ag-ui-backend)))

(deftest run-agent-needs-rpc-transport
  (let ((rpc-protocol:*rpc-transport* nil)
        (backend (make-instance 'ag-ui-protocol:ag-ui-backend)))
    (ok (signals (ag-ui-protocol:run-agent backend nil)
                 'ag-ui-protocol:ag-ui-error))))
