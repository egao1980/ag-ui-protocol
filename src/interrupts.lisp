(in-package #:ag-ui-protocol)

;;; Resume validation.
;;;
;;; The interrupt contract puts real obligations on both sides, and every
;;; violation is specified to surface as RUN_ERROR rather than a silent
;;; mis-resume. Agents check inbound resumes here; clients can run the same
;;; check before submitting, which is the point of sharing one implementation.

(define-condition ag-ui-resume-error (ag-ui-error)
  ((rule :initarg :rule :initform nil :reader ag-ui-resume-error-rule))
  (:documentation "A resume violated the interrupt contract. RULE names which."))

(defun %resume-error (rule control &rest args)
  (error 'ag-ui-resume-error :rule rule :message (apply #'format nil control args)))

;;; ISO-8601 instants, only as far as expiry comparison needs. Parsing this
;;; inline keeps a whole datetime dependency out of a wire-protocol repo.

(defun %digits (string start end)
  (let ((n 0))
    (loop for i from start below end
          for ch = (char string i)
          unless (digit-char-p ch) do (return-from %digits nil)
            do (setf n (+ (* n 10) (digit-char-p ch))))
    n))

(defun parse-iso8601 (string)
  "STRING as a universal time, or NIL if it is not an instant we can read.
   Accepts YYYY-MM-DDTHH:MM:SS with optional fractional seconds and either Z or
   a +HH:MM / -HH:MM offset."
  (when (and (stringp string) (>= (length string) 19))
    (let ((year (%digits string 0 4))
          (month (%digits string 5 7))
          (day (%digits string 8 10))
          (hour (%digits string 11 13))
          (minute (%digits string 14 16))
          (second (%digits string 17 19)))
      (when (and year month day hour minute second
                 (<= 1 month 12) (<= 1 day 31)
                 (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 60))
        (let* ((rest (subseq string 19))
               (sign-pos (position-if (lambda (c) (member c '(#\+ #\-))) rest))
               (offset-seconds
                 (cond
                   (sign-pos
                    (let ((oh (%digits rest (1+ sign-pos) (+ sign-pos 3)))
                          (om (or (%digits rest (+ sign-pos 4) (+ sign-pos 6)) 0)))
                      (and oh (* (if (char= (char rest sign-pos) #\-) -1 1)
                                 (+ (* oh 3600) (* om 60))))))
                   (t 0))))
          (when offset-seconds
            ;; ENCODE-UNIVERSAL-TIME rejects second 60; clamp the leap second.
            (- (encode-universal-time (min second 59) minute hour day month year 0)
               offset-seconds)))))))

(defun interrupt-expired-p (interrupt &optional (now (get-universal-time)))
  "Has INTERRUPT's expiresAt passed? NIL when it declares no expiry, and NIL
   when the value is unparseable — refusing a resume over a timestamp we could
   not read would be worse than letting the agent adjudicate it."
  (let* ((raw (event-field interrupt 'expires-at))
         (deadline (and raw (parse-iso8601 raw))))
    (and deadline (> now deadline))))

(defun open-interrupts (event)
  "Interrupts carried by a RUN_FINISHED with an interrupt outcome, else NIL."
  (let ((outcome (and (typep event 'run-finished-event)
                      (event-field event 'outcome))))
    (when (typep outcome 'run-interrupt-outcome)
      (coerce (or (outcome-interrupts outcome) #()) 'list))))

(defun run-interrupted-p (event)
  (not (null (open-interrupts event))))

(defun validate-resume (interrupts resume &key (now (get-universal-time))
                                            (validate-payloads t))
  "Check RESUME entries against the INTERRUPTS they answer.

   Signals AG-UI-RESUME-ERROR on the conditions the spec says must produce
   RUN_ERROR: an entry naming an interrupt that was not open, an interrupt left
   unanswered, a resume arriving past expiresAt, or a payload that fails its
   responseSchema. Returns T."
  (let* ((interrupts (%as-list interrupts))
         (entries (%as-list resume))
         (open-ids (mapcar #'interrupt-id interrupts))
         (answered '()))
    (dolist (entry entries)
      (let* ((id (resume-interrupt-id entry))
             (interrupt (find id interrupts :key #'interrupt-id :test #'equal)))
        (unless interrupt
          (%resume-error :unknown-interrupt
                         "resume references unknown interrupt ~a" id))
        (when (member id answered :test #'equal)
          (%resume-error :duplicate-resume
                         "resume answers interrupt ~a more than once" id))
        (push id answered)
        (when (interrupt-expired-p interrupt now)
          (%resume-error :expired
                         "interrupt ~a expired at ~a" id
                         (event-field interrupt 'expires-at)))
        (when (equal (resume-status entry) "resolved")
          (let ((schema (event-field interrupt 'response-schema))
                (payload (event-field entry 'payload)))
            (when (and validate-payloads schema)
              (handler-case
                  (stack-schema-json:validate-instance schema payload)
                (stack-schema-json:json-schema-validation-error (c)
                  (%resume-error :payload
                                 "resume payload for ~a fails its responseSchema: ~a"
                                 id c))))))))
    ;; Partial resumes are not supported: every open interrupt must be addressed.
    (let ((missing (remove-if (lambda (id) (member id answered :test #'equal))
                              open-ids)))
      (when missing
        (%resume-error :incomplete
                       "resume leaves interrupt~p unanswered: ~{~a~^ ~}"
                       (length missing) missing)))
    t))

(defun validate-resume-input (input interrupts &key (now (get-universal-time)))
  "Check INPUT against the interrupts left open by the previous run.

   Enforces the rule that pending interrupts block new input: an input on a
   thread with open interrupts must carry a resume addressing them."
  (let ((interrupts (%as-list interrupts))
        (resume (%as-list (event-field input 'resume))))
    (cond
      ((and interrupts (null resume))
       (%resume-error :resume-required
                      "thread ~a has ~a open interrupt~:p; input must carry resume"
                      (run-agent-input-thread-id input) (length interrupts)))
      ((and (null interrupts) resume)
       (%resume-error :nothing-to-resume
                      "input carries resume but no interrupts are open"))
      (interrupts (validate-resume interrupts resume :now now))
      (t t))))

(defun resume-approved-p (entry)
  "Did this entry approve? `cancelled` never approves; otherwise the decision
   lives in the payload's `approved` field, defaulting to true for a resolved
   answer that carries no explicit verdict."
  (and (equal (resume-status entry) "resolved")
       (let ((payload (event-field entry 'payload)))
         (if (hash-table-p payload)
             (multiple-value-bind (value found) (gethash "approved" payload)
               (if found (and value (not (eq value :false))) t))
             t))))

(defun resume-edited-args (entry)
  "Replacement tool arguments from an approve-with-edits payload, or NIL.
   A full replacement, never merged into the original arguments."
  (let ((payload (event-field entry 'payload)))
    (and (hash-table-p payload) (gethash "editedArgs" payload))))
