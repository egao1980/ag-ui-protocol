(defsystem "ag-ui-protocol"
  :version "0.1.0"
  :description "CLOS AG-UI (agent-user events) protocol for cl-stack"
  :author "egao1980"
  :license "MIT"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
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
