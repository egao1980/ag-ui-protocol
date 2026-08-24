(in-package #:ag-ui-protocol)

(defclass ag-ui-event ()
  ((type :initarg :type :accessor ag-ui-event-type)
   (timestamp :initarg :timestamp :initform nil)))

(defclass run-agent-input () ())
(defclass ag-ui-backend () ())

(defvar *ag-ui-backend* nil)

(defgeneric run-agent (backend input &key on-event transport))
(defgeneric encode-ag-ui-event (event &key format))
(defgeneric decode-ag-ui-event (source &key format))
(defgeneric serve-ag-ui (backend &key path transport))

(defun %ensure-backend (&optional (backend *ag-ui-backend*))
  (or backend
      (error 'ag-ui-error :message "*ag-ui-backend* is nil — load ag-ui-backend-sse")))

(defun %ensure-transport (&optional (transport rpc-protocol:*rpc-transport*))
  (or transport
      (error 'ag-ui-error
             :message "no rpc-transport — load rpc-backend-sse (or another rpc-backend-*)")))

(defmethod run-agent ((backend ag-ui-backend) input &key on-event
                      (transport rpc-protocol:*rpc-transport*))
  "Official AG-UI HTTP is rpc-protocol :call-stream — never a JSON-RPC envelope."
  (let ((stream (rpc-protocol:rpc-call-stream
                 "RunAgent" input :transport (%ensure-transport transport))))
    (unwind-protect
         (loop for ev = (rpc-protocol:rpc-recv stream)
               until (eq ev :eof)
               do (when on-event (funcall on-event ev))
               collect ev)
      (rpc-protocol:rpc-close stream))))

(defmethod serve-ag-ui ((backend ag-ui-backend) &key path
                        (transport rpc-protocol:*rpc-transport*))
  (declare (ignore path))
  (rpc-protocol:rpc-serve-stream
   (lambda (mode method params stream)
     (declare (ignore mode method))
     (dolist (ev (run-agent backend params))
       (rpc-protocol:rpc-send stream ev)))
   :transport (%ensure-transport transport)))
