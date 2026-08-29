(defsystem "ag-ui-protocol"
  :version "0.2.1"
  :description "CLOS AG-UI protocol — typed agent↔UI events (not JSON-RPC)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("yason" "babel" "sse-protocol"
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
               (:file "protocol"))
  :in-order-to ((test-op (test-op "ag-ui-protocol/tests"))))

(defsystem "ag-ui-protocol/tests"
  :depends-on ("ag-ui-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
