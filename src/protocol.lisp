(in-package #:ag-ui-protocol)

;;; Official HTTP is POST RunAgentInput → SSE of typed events.
;;; Not JSON-RPC — do not route through rpc-protocol-json.

(defgeneric run-agent (backend input &key on-event)
  (:documentation "Run INPUT on BACKEND (local agent or HTTP client)."))

(defgeneric serve-ag-ui (backend &key path host port)
  (:documentation "Serve BACKEND as POST path → text/event-stream."))

(defun %ensure-input (input)
  (cond
    ((typep input 'run-agent-input) input)
    ((or (hash-table-p input) (stringp input))
     (decode-run-agent-input input))
    (t (error 'ag-ui-error :message "run-agent needs a run-agent-input"))))

(defun last-user-text (input)
  "Content of the last user message, or \"\"."
  (let ((text ""))
    (dolist (m (run-agent-input-messages input) text)
      (when (and (string= (ag-ui-message-role m) "user")
                 (stringp (ag-ui-message-content m)))
        (setf text (ag-ui-message-content m))))))

(defun echo-handler (input)
  "Wave-1 echo: RUN_STARTED → TEXT_MESSAGE_* of last user text → RUN_FINISHED."
  (let* ((thread (or (run-agent-input-thread-id input) "thread"))
         (run (or (run-agent-input-run-id input) "run"))
         (text (last-user-text input))
         (mid "msg-echo"))
    (list (make-run-started-event :thread-id thread :run-id run)
          (make-text-message-start-event :message-id mid :role "assistant")
          (make-text-message-content-event :message-id mid :delta text)
          (make-text-message-end-event :message-id mid)
          (make-run-finished-event :thread-id thread :run-id run))))

(defmethod run-agent ((agent ag-ui-agent) input &key on-event)
  (let* ((input (%ensure-input input))
         (fn (or (ag-ui-agent-handler agent) #'echo-handler))
         (events (funcall fn input)))
    (when on-event
      (mapc on-event events))
    events))

(defmethod run-agent ((backend ag-ui-backend) input &key on-event)
  (declare (ignore input on-event))
  (error 'ag-ui-error
         :message "ag-ui-backend has no transport — load ag-ui-backend-sse"))

(defmethod serve-ag-ui ((backend ag-ui-backend) &key path host port)
  (declare (ignore path host port))
  (error 'ag-ui-error
         :message "serve-ag-ui needs ag-ui-backend-sse (or protobuf)"))

(defun %slurp-raw-body (raw)
  (cond
    ((null raw) "")
    ((stringp raw) raw)
    ((and (vectorp raw) (not (stringp raw)))
     (babel:octets-to-string raw :encoding :utf-8))
    ((streamp raw)
     (if (ignore-errors
           (let ((et (stream-element-type raw)))
             (and et (subtypep et 'character))))
         (with-output-to-string (out)
           (loop for c = (read-char raw nil :eof)
                 until (eq c :eof)
                 do (write-char c out)))
         (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer 0)))
           (loop for b = (read-byte raw nil :eof)
                 until (eq b :eof)
                 do (vector-push-extend b bytes))
           (babel:octets-to-string bytes :encoding :utf-8))))
    (t (princ-to-string raw))))

(defun make-ag-ui-app (agent &key (path "/") (event-format :json))
  "Clack app: POST PATH with RunAgentInput JSON → SSE of typed events.
   Other methods/paths → 404. EVENT-FORMAT is :json or :protobuf (JSON octets)."
  (lambda (env)
    (let ((req-path (or (getf env :path-info) "/"))
          (method (getf env :request-method)))
      (cond
        ((and (eq method :post) (string= req-path path))
         (handler-case
             (let* ((input (decode-run-agent-input
                            (%slurp-raw-body (getf env :raw-body))))
                    (events (run-agent agent input))
                    (body (mapcar (lambda (ev)
                                    (encode-ag-ui-sse ev :format event-format))
                                  events)))
               (list 200
                     '(:content-type "text/event-stream; charset=utf-8"
                       :cache-control "no-cache")
                     body))
           (ag-ui-error (c)
             (list 400
                   '(:content-type "application/json; charset=utf-8")
                   (list (encode-json
                          (json-object "error" (ag-ui-error-message c))))))))
        (t
         '(404 (:content-type "text/plain; charset=utf-8") ("not found")))))))
