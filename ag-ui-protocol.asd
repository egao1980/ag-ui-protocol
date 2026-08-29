(defsystem "ag-ui-protocol"
  :version "0.3.0"
  :description "CLOS AG-UI protocol — typed agent↔UI events (not JSON-RPC)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("yason" "babel" "sse-protocol" "serdes-protocol"
               "schema-protocol" "schema-protocol-json")
  :properties (:cl-repo (:ci (:with ("dissect"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "json")
               (:file "events")
               (:file "chunks")
               (:file "interrupts")
               (:file "capabilities")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "ag-ui-protocol/tests"))))

;;; Client-side runtime: ordering verification plus the reducer that folds a
;;; stream into messages and state. Separate so a server-only consumer does not
;;; pay for it, and so it can take the JSON Patch dependency STATE_DELTA needs.
(defsystem "ag-ui-protocol/client"
  :version "0.1.0"
  :description "AG-UI client runtime — verify event ordering, reduce to messages + state"
  :author "egao1980"
  :license "MIT"
  :depends-on ("ag-ui-protocol" "json-patch")
  :serial t
  :pathname "client"
  :components ((:file "package")
               (:file "verify")
               (:file "apply"))
  :in-order-to ((test-op (test-op "ag-ui-protocol/tests"))))

;;; Optional bridge to capability-protocol. Separate so a wire-protocol consumer
;;; does not pull in blackboard-protocol; same shape as llm-protocol/capability.
(defsystem "ag-ui-protocol/capability"
  :version "0.1.0"
  :description "capability-protocol ↔ AG-UI AgentCapabilities adapters"
  :author "egao1980"
  :license "MIT"
  :depends-on ("ag-ui-protocol" "capability-protocol")
  :serial t
  :pathname "capability"
  :components ((:file "package")
               (:file "adapter"))
  :in-order-to ((test-op (test-op "ag-ui-protocol/tests"))))

(defsystem "ag-ui-protocol/tests"
  :depends-on ("ag-ui-protocol" "ag-ui-protocol/client"
               "ag-ui-protocol/capability"
               (:version "protobuf-backend-cl-protobufs" "0.2.0") "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "client-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
