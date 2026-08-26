(in-package #:ag-ui-protocol)

;;; Conditions + restarts around RUN-AGENT.

(define-condition ag-ui-error (error)
  ((message :initarg :message :reader ag-ui-error-message :initform nil))
  (:report (lambda (c s)
             (format s "ag-ui error~@[: ~a~]" (ag-ui-error-message c)))))

(define-condition ag-ui-run-error (ag-ui-error)
  ((code :initarg :code :reader ag-ui-run-error-code :initform nil))
  (:report (lambda (c s)
             (format s "ag-ui run error~@[ [~a]~]~@[: ~a~]"
                     (ag-ui-run-error-code c)
                     (ag-ui-error-message c)))))

(defun call-with-ag-ui-restarts (thunk)
  "Establish RETRY / USE-VALUE around THUNK."
  (tagbody
   :retry
     (return-from call-with-ag-ui-restarts
       (restart-case (funcall thunk)
         (retry ()
           :report "Retry RUN-AGENT"
           (go :retry))
         (use-value (value)
           :report "Use a supplied event list instead"
           :interactive (lambda ()
                          (format *query-io* "Events: ")
                          (force-output *query-io*)
                          (list (read *query-io*)))
           value)))))

(defmacro with-ag-ui-restarts (&body body)
  `(call-with-ag-ui-restarts (lambda () ,@body)))

(defun invoke-retry (&optional condition)
  (let ((r (find-restart 'retry condition)))
    (when r (invoke-restart r))))

(defun invoke-use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun auto-retry (condition)
  (if (find-restart 'retry condition)
      (invoke-retry condition)
      (error condition)))

(defmacro with-auto-retry (&body body)
  `(handler-bind ((ag-ui-error #'auto-retry))
     (with-ag-ui-restarts ,@body)))
