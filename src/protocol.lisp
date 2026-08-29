(in-package #:ag-ui-protocol)

;;; Official HTTP is POST RunAgentInput → SSE of typed events.
;;; Not JSON-RPC — do not route through rpc-protocol-json.

(defgeneric run-agent (backend input &key on-event)
  (:documentation "Run INPUT on BACKEND (local agent or HTTP client)."))

(defmethod run-agent :around (backend input &rest args)
  (declare (ignore backend input args))
  (with-ag-ui-restarts (call-next-method)))

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
    (map nil (lambda (m)
               (when (and (string= (ag-ui-message-role m) "user")
                          (stringp (ag-ui-message-content m)))
                 (setf text (ag-ui-message-content m))))
         (or (run-agent-input-messages input) #()))
    text))

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

(defvar *ag-ui-emit* nil
  "Bound by RUN-AGENT to (lambda (ag-ui-event)). Incremental handlers call AG-UI-EMIT.")

(defun ag-ui-emit (event)
  "Push EVENT to the current RUN-AGENT sink, if any."
  (when *ag-ui-emit*
    (funcall *ag-ui-emit* event))
  event)

(defmethod run-agent ((agent ag-ui-agent) input &key on-event)
  (let* ((input (%ensure-input input))
         (fn (or (ag-ui-agent-handler agent) #'echo-handler))
         (collected '())
         (emitted-p nil)
         (*ag-ui-emit*
          (lambda (event)
            (setf emitted-p t)
            (setf collected (nconc collected (list event)))
            (when on-event (funcall on-event event)))))
    (let ((events (funcall fn input)))
      (cond
        (emitted-p collected)
        (t
         (when on-event (mapc on-event events))
         events)))))

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

(defparameter +ag-ui-sse-media-type+ "text/event-stream")
(defparameter +ag-ui-proto-media-type+ "application/vnd.ag-ui.event+proto")

(defun %split-comma (string)
  (loop for start = 0 then (1+ comma)
        for comma = (position #\, string :start start)
        collect (subseq string start (or comma (length string)))
        while comma))

(defun %accept-q (params)
  (let ((at (search "q=" params)))
    (if (null at)
        1.0
        (let* ((rest (subseq params (+ at 2)))
               (end (or (position #\; rest) (length rest)))
               (n (handler-case
                      (let ((*read-eval* nil))
                        (read-from-string (subseq rest 0 end)))
                    (error () nil))))
          (if (realp n) (float n 1.0) 0.0)))))

(defun %parse-accept (header)
  (when (and header (plusp (length (string-trim '(#\space #\tab) header))))
    (loop for part in (%split-comma header)
          for trimmed = (string-trim '(#\space #\tab) part)
          for semi = (position #\; trimmed)
          for media = (string-downcase
                       (string-trim '(#\space #\tab)
                                    (if semi (subseq trimmed 0 semi) trimmed)))
          for q = (if semi (%accept-q (subseq trimmed (1+ semi))) 1.0)
          collect (cons media q))))

(defun negotiate-ag-ui-format (accept &key (protobuf-available-p (%wkt-available-p)))
  "→ :json, :protobuf, or NIL (406).

   Protobuf is chosen only when `application/vnd.ag-ui.event+proto` is explicit
   with q>0 and a :wkt serdes backend is loaded. Otherwise SSE when
   text/event-stream, text/*, */*, or Accept is absent."
  (let ((parts (%parse-accept accept)))
    (flet ((q (type)
             (or (cdr (assoc type parts :test #'string=)) 0)))
      (cond
        ((and protobuf-available-p (plusp (q +ag-ui-proto-media-type+)))
         :protobuf)
        ((or (null parts)
             (plusp (q +ag-ui-sse-media-type+))
             (plusp (q "text/*"))
             (plusp (q "*/*")))
         :json)
        (t nil)))))

(defun %env-header (env name)
  (let ((headers (getf env :headers)))
    (cond
      ((hash-table-p headers)
       (or (gethash name headers)
           (gethash (string-downcase name) headers)))
      ((listp headers)
       (or (getf headers (intern (string-upcase name) :keyword))
           (cdr (assoc name headers :test #'string-equal))))
      (t nil))))

(defun %run-protobuf-body (agent input)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (run-agent agent input
               :on-event
               (lambda (ev)
                 (loop for b across (encode-ag-ui-framed ev)
                       do (vector-push-extend b out))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %app-capabilities (agent)
  (or (get-capabilities agent)
      (make-agent-capabilities
       :identity (%make 'identity-capabilities :name (ag-ui-agent-name agent))
       :transport (%make 'transport-capabilities
                         :streaming t
                         :http-binary (%wkt-available-p)))))

(defun make-ag-ui-app (agent &key (path "/") (event-format :negotiate))
  "Clack app: POST PATH with RunAgentInput JSON → event stream.
   GET PATH → AgentCapabilities JSON.
   EVENT-FORMAT is :negotiate (Accept), :json, or :protobuf."
  (lambda (env)
    (let ((req-path (or (getf env :path-info) "/"))
          (method (getf env :request-method)))
      (cond
        ((and (eq method :get) (string= req-path path))
         (list 200
               '(:content-type "application/json; charset=utf-8")
               (list (encode-json
                      (encode-agent-capabilities (%app-capabilities agent))))))
        ((and (eq method :post) (string= req-path path))
         (handler-case
             (let* ((accept (%env-header env "accept"))
                    (format (ecase event-format
                              (:negotiate (negotiate-ag-ui-format accept))
                              (:json :json)
                              (:protobuf
                               (if (%wkt-available-p) :protobuf nil))))
                    (input (decode-run-agent-input
                            (%slurp-raw-body (getf env :raw-body)))))
               (cond
                 ((eq format :protobuf)
                  (list 200
                        (list :content-type +ag-ui-proto-media-type+
                              :cache-control "no-cache")
                        (list (%run-protobuf-body agent input))))
                 ((eq format :json)
                  (list 200
                        '(:content-type "text/event-stream; charset=utf-8"
                          :cache-control "no-cache")
                        (lambda (stream)
                          (run-agent agent input
                                     :on-event
                                     (lambda (ev)
                                       (write-string (encode-ag-ui-sse ev) stream)
                                       (force-output stream))))))
                 (t
                  '(406 (:content-type "text/plain; charset=utf-8")
                    ("not acceptable")))))
           (ag-ui-error (c)
             (list 400
                   '(:content-type "application/json; charset=utf-8")
                   (list (encode-json
                          (json-object "error" (ag-ui-error-message c))))))))
        (t
         '(404 (:content-type "text/plain; charset=utf-8") ("not found")))))))
