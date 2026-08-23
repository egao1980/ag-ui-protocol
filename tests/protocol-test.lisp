(in-package #:ag-ui-protocol/tests)

(deftest classes-exist
  (ok (find-class 'ag-ui-protocol:ag-ui-event))
  (ok (find-class 'ag-ui-protocol:run-agent-input))
  (ok (find-class 'ag-ui-protocol:ag-ui-backend)))
