(in-package #:ag-ui-protocol)

(define-condition ag-ui-error (error)
  ((message :initarg :message :reader ag-ui-error-message :initform nil))
  (:report (lambda (c s)
             (format s "ag-ui error~@[: ~a~]" (ag-ui-error-message c)))))
