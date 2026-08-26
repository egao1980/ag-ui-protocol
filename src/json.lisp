(in-package #:ag-ui-protocol)

(defun json-object (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          unless (or (null k) (eq v :omit))
            do (setf (gethash k h) v))
    h))

(defun param (obj key &optional default)
  (cond
    ((null obj) default)
    ((hash-table-p obj) (gethash key obj default))
    ((listp obj)
     (let ((cell (assoc key obj :test #'equal)))
       (if cell (cdr cell) default)))
    (t default)))

(defun encode-json (obj)
  (let ((yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase))
    (with-output-to-string (s)
      (yason:encode obj s))))

(defun decode-json (string)
  (yason:parse string :object-as :hash-table :json-arrays-as-vectors t))

(defun %as-list (seq)
  (cond
    ((null seq) nil)
    ((vectorp seq) (coerce seq 'list))
    ((listp seq) seq)
    (t (list seq))))

(defun %as-vector (seq)
  (cond
    ((null seq) #())
    ((vectorp seq) seq)
    (t (coerce seq 'vector))))

(defun %source-string (source)
  (cond
    ((stringp source) source)
    ((and (vectorp source) (not (stringp source)))
     (babel:octets-to-string source :encoding :utf-8))
    ((streamp source)
     (with-output-to-string (out)
       (loop for c = (read-char source nil :eof)
             until (eq c :eof)
             do (write-char c out))))
    (t (error 'ag-ui-error :message "cannot coerce source to string"))))

(defun encode-message (message)
  (json-object
   "id" (ag-ui-message-id message)
   "role" (ag-ui-message-role message)
   "content" (or (ag-ui-message-content message) :omit)
   "name" (or (ag-ui-message-name message) :omit)
   "toolCallId" (or (ag-ui-message-tool-call-id message) :omit)
   "toolCalls" (if (ag-ui-message-tool-calls message)
                   (%as-vector (ag-ui-message-tool-calls message))
                   :omit)))

(defun decode-message (obj)
  (make-ag-ui-message
   :id (or (param obj "id") (param obj "messageId"))
   :role (or (param obj "role") "user")
   :content (param obj "content")
   :name (param obj "name")
   :tool-call-id (or (param obj "toolCallId") (param obj "tool_call_id"))
   :tool-calls (%as-list (param obj "toolCalls"))))

(defun encode-tool (tool)
  (json-object
   "name" (ag-ui-tool-name tool)
   "description" (ag-ui-tool-description tool)
   "parameters" (or (ag-ui-tool-parameters tool) (json-object))))

(defun decode-tool (obj)
  (make-ag-ui-tool
   :name (param obj "name")
   :description (or (param obj "description") "")
   :parameters (param obj "parameters")))

(defun encode-context (ctx)
  (json-object
   "description" (ag-ui-context-description ctx)
   "value" (ag-ui-context-value ctx)))

(defun decode-context (obj)
  (make-ag-ui-context
   :description (param obj "description")
   :value (param obj "value")))

(defun encode-run-agent-input (input)
  (json-object
   "threadId" (run-agent-input-thread-id input)
   "runId" (run-agent-input-run-id input)
   "parentRunId" (or (run-agent-input-parent-run-id input) :omit)
   "state" (or (run-agent-input-state input) :omit)
   "messages" (map 'vector #'encode-message
                   (or (run-agent-input-messages input) #()))
   "tools" (map 'vector #'encode-tool (or (run-agent-input-tools input) #()))
   "context" (map 'vector #'encode-context (or (run-agent-input-context input) #()))
   "forwardedProps" (or (run-agent-input-forwarded-props input) (json-object))))

(defun decode-run-agent-input (source)
  (let ((obj (if (hash-table-p source) source (decode-json (%source-string source)))))
    (make-run-agent-input
     :thread-id (or (param obj "threadId") (param obj "thread_id"))
     :run-id (or (param obj "runId") (param obj "run_id"))
     :parent-run-id (or (param obj "parentRunId") (param obj "parent_run_id"))
     :state (let ((st (param obj "state")))
              (if (eq st :null) nil st))
     :messages (mapcar #'decode-message (%as-list (param obj "messages")))
     :tools (mapcar #'decode-tool (%as-list (param obj "tools")))
     :context (mapcar #'decode-context (%as-list (param obj "context")))
     :forwarded-props (param obj "forwardedProps"))))
