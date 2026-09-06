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
     (encoding-protocol:decode raw))
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
           (encoding-protocol:decode bytes))))
    (t (princ-to-string raw))))

(defparameter +ag-ui-sse-media-type+ "text/event-stream")
(defparameter +ag-ui-proto-media-type+ "application/vnd.ag-ui.event+proto")
(defparameter +ag-ui-oneof-media-type+ "application/vnd.ag-ui.event+oneof")

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

(defun negotiate-ag-ui-format (accept &key
                                     (protobuf-available-p (%wkt-available-p))
                                     (oneof-available-p (%oneof-available-p)))
  "→ :json, :protobuf (WKT), :oneof (official Event), or NIL (406).

   Binary types are chosen only when named explicitly with q>0. WKT stays on
   `application/vnd.ag-ui.event+proto`. Official Event oneof is
   `application/vnd.ag-ui.event+oneof` — the spec reuses +proto for oneof, but
   that media type already means WKT here; do not silently replace it.
   First matching explicit type in Accept wins. Otherwise SSE when
   text/event-stream, text/*, */*, or Accept is absent."
  (let ((parts (%parse-accept accept)))
    (flet ((q (type)
             (or (cdr (assoc type parts :test #'string=)) 0)))
      (dolist (pair parts)
        (cond
          ((and (string= (car pair) +ag-ui-oneof-media-type+)
                (plusp (cdr pair))
                oneof-available-p)
           (return-from negotiate-ag-ui-format :oneof))
          ((and (string= (car pair) +ag-ui-proto-media-type+)
                (plusp (cdr pair))
                protobuf-available-p)
           (return-from negotiate-ag-ui-format :protobuf))))
      (cond
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

(defun %concat-chunks (chunks)
  (cond
    ((null chunks) #())
    ((every (lambda (c) (and (vectorp c) (not (stringp c)))) chunks)
     (apply #'concatenate '(vector (unsigned-byte 8)) chunks))
    (t (apply #'concatenate 'string
              (mapcar (lambda (c)
                        (if (stringp c)
                            c
                            (encoding-protocol:decode c)))
                      chunks)))))

(defun invoke-ag-ui-app (app env)
  "Call APP and return a materialized Clack 3-list.
   Drains a response function (binary frames stay octets) or a body writer."
  (let ((res (funcall app env)))
    (cond
      ((functionp res)
       (let ((status nil) (headers nil) (chunks '()))
         (funcall res
                  (lambda (status-and-headers)
                    (setf status (first status-and-headers)
                          headers (second status-and-headers))
                    (lambda (body &key (start 0) end close)
                      (declare (ignore close))
                      (when body
                        (push (subseq body start (or end (length body)))
                              chunks))
                      (values))))
         (list status headers (list (%concat-chunks (nreverse chunks))))))
      ((and (consp res) (functionp (third res)))
       (list (first res) (second res)
             (list (with-output-to-string (s)
                     (funcall (third res) s)))))
      (t res))))

(defun %clack-binary-response (status headers write-fn)
  "Clack response function. WRITE-FN receives (lambda (octets)).
   Writes octet vectors to the responder — never PRINC (Hunchentoot footgun)."
  (lambda (responder)
    (let ((writer (funcall responder (list status headers))))
      (funcall write-fn
               (lambda (octets)
                 (when (and octets (plusp (length octets)))
                   (funcall writer octets))))
      (ignore-errors (funcall writer nil :close t)))))

(defun %stream-framed-events (agent input encode-frame)
  (lambda (write-octets)
    (run-agent agent input
               :on-event
               (lambda (ev)
                 (funcall write-octets (funcall encode-frame ev))))))

(defun %app-capabilities (agent)
  (or (get-capabilities agent)
      (make-agent-capabilities
       :identity (%make 'identity-capabilities :name (ag-ui-agent-name agent))
       :transport (%make 'transport-capabilities
                         :streaming t
                         :http-binary (or (%wkt-available-p)
                                          (%oneof-available-p))))))

(defun make-ag-ui-app (agent &key (path "/") (event-format :negotiate))
  "Clack app: POST PATH with RunAgentInput JSON → event stream.
   GET PATH → AgentCapabilities JSON.
   EVENT-FORMAT is :negotiate (Accept), :json, :protobuf (WKT), or :oneof."
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
                               (if (%wkt-available-p) :protobuf nil))
                              (:oneof
                               (if (%oneof-available-p) :oneof nil))))
                    (input (decode-run-agent-input
                            (%slurp-raw-body (getf env :raw-body)))))
               (cond
                 ((eq format :protobuf)
                  (%clack-binary-response
                   200
                   (list :content-type +ag-ui-proto-media-type+
                         :cache-control "no-cache")
                   (%stream-framed-events agent input #'encode-ag-ui-framed)))
                 ((eq format :oneof)
                  (%clack-binary-response
                   200
                   (list :content-type +ag-ui-oneof-media-type+
                         :cache-control "no-cache")
                   (%stream-framed-events agent input #'encode-ag-ui-framed-oneof)))
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
