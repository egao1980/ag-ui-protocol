(in-package #:ag-ui-protocol)

(defclass ag-ui-event ()
  ((type :initarg :type :accessor ag-ui-event-type)
   (timestamp :initarg :timestamp :initform nil)))

(defclass run-agent-input () ())
(defclass ag-ui-backend () ())

(defvar *ag-ui-backend* nil)

(defgeneric run-agent (backend input &key on-event))
(defgeneric encode-ag-ui-event (event &key format))
(defgeneric decode-ag-ui-event (source &key format))
(defgeneric serve-ag-ui (backend &key path))

(defun %ensure-backend (&optional (backend *ag-ui-backend*))
  (or backend
      (error 'ag-ui-error :message "*ag-ui-backend* is nil — load ag-ui-backend-sse")))
