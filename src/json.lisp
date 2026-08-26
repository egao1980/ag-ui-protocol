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

(defun %as-list (seq)
  (cond
    ((null seq) nil)
    ((vectorp seq) (coerce seq 'list))
    ((listp seq) seq)
    (t (list seq))))

(defun %schema-error (c)
  (error 'ag-ui-error
         :message (or (ignore-errors
                        (princ-to-string
                         (first (stack-schema:schema-validation-error-issues c))))
                      (princ-to-string c))))

(defun encode-run-agent-input (input)
  (stack-schema:dump input))

(defun decode-run-agent-input (source)
  (handler-case
      (stack-schema:parse
       'run-agent-input
       (if (or (hash-table-p source) (listp source))
           source
           (decode-json (%source-string source))))
    (stack-schema:schema-validation-error (c)
      (%schema-error c))))

(defun ag-ui-json-schema (schema &key (draft :draft-07))
  "Draft-07 JSON Schema for a model (events, RunAgentInput, tools, …)."
  (stack-schema:json-schema schema :draft draft))

(defun validate-ag-ui-json (source &key (schema 'ag-ui-event))
  "Validate JSON (string or table) against SCHEMA's emitted JSON Schema, then parse."
  (let ((obj (if (or (hash-table-p source) (listp source))
                 source
                 (decode-json (%source-string source)))))
    (handler-case
        (stack-schema-json:validate-instance (ag-ui-json-schema schema) obj)
      (stack-schema-json:json-schema-validation-error (c)
        (error 'ag-ui-error :message (princ-to-string c))))
    (stack-schema:parse schema obj)))

(defun validate-tool-arguments (tool arguments)
  "Validate ARGUMENTS (hash-table / JSON string) against TOOL.parameters JSON Schema.
   No-op when the tool has no parameters. Returns the parsed table."
  (let ((params (ag-ui-tool-parameters tool))
        (args (if (or (hash-table-p arguments) (listp arguments))
                  arguments
                  (decode-json (%source-string arguments)))))
    (unless params
      (return-from validate-tool-arguments args))
    (handler-case
        (stack-schema-json:validate-instance params args)
      (stack-schema-json:json-schema-validation-error (c)
        (error 'ag-ui-error :message (princ-to-string c))))
    args))
