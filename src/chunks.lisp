(in-package #:ag-ui-protocol)

;;; Chunk expansion — the CL counterpart of @ag-ui/client's transformChunks.
;;;
;;; *_CHUNK events let a producer skip the explicit START/END brackets. The
;;; boundaries are implied: the first chunk of an id opens it, a later chunk
;;; with a different id closes the previous one, and end-of-stream closes
;;; whatever is still open. Expanding here means every downstream reducer,
;;; verifier, and transcript sees the triad form and never has to care which
;;; spelling the producer chose.

(defclass chunk-expander ()
  ((text-id :initform nil :accessor expander-text-id)
   (tool-id :initform nil :accessor expander-tool-id)
   (reasoning-id :initform nil :accessor expander-reasoning-id))
  (:documentation "Open-chunk state for one event stream. Not thread-safe;
   one expander belongs to one run."))

(defun make-chunk-expander ()
  (make-instance 'chunk-expander))

(defun %chunk-error (control &rest args)
  (error 'ag-ui-error :message (apply #'format nil control args)))

(defun %close-text (expander)
  (let ((id (expander-text-id expander)))
    (when id
      (setf (expander-text-id expander) nil)
      (list (make-text-message-end-event :message-id id)))))

(defun %close-tool (expander)
  (let ((id (expander-tool-id expander)))
    (when id
      (setf (expander-tool-id expander) nil)
      (list (make-tool-call-end-event :tool-call-id id)))))

(defun %close-reasoning (expander)
  (let ((id (expander-reasoning-id expander)))
    (when id
      (setf (expander-reasoning-id expander) nil)
      (list (make-reasoning-message-end-event :message-id id)))))

(defgeneric expand-chunk (expander event)
  (:documentation "Expand EVENT against EXPANDER, returning a list of events.

   Non-chunk events pass through, closing any open chunk sequence they
   supersede. Chunk events expand to their START / CONTENT / END equivalents."))

(defmethod expand-chunk ((expander chunk-expander) (event ag-ui-event))
  ;; Default: any event that is not part of the reasoning family implicitly
  ;; closes an open reasoning message. Families that manage their own state
  ;; override this.
  (append (%close-reasoning expander) (list event)))

(defmethod expand-chunk ((expander chunk-expander) (event text-message-chunk-event))
  (let ((id (or (event-field event 'message-id) (expander-text-id expander)))
        (out '()))
    (unless id
      (%chunk-error "TEXT_MESSAGE_CHUNK needs messageId on the first chunk"))
    (unless (equal id (expander-text-id expander))
      (setf out (append out (%close-text expander) (%close-reasoning expander)))
      (setf out (append out (list (make-text-message-start-event
                                   :message-id id
                                   :role (or (event-field event 'role) "assistant")))))
      (setf (expander-text-id expander) id))
    (let ((delta (event-field event 'delta)))
      (when (and delta (plusp (length delta)))
        (setf out (append out (list (make-text-message-content-event
                                     :message-id id :delta delta))))))
    out))

(defmethod expand-chunk ((expander chunk-expander) (event tool-call-chunk-event))
  (let ((id (or (event-field event 'tool-call-id) (expander-tool-id expander)))
        (out '()))
    (unless id
      (%chunk-error "TOOL_CALL_CHUNK needs toolCallId on the first chunk"))
    (unless (equal id (expander-tool-id expander))
      (let ((name (event-field event 'tool-call-name)))
        (unless name
          (%chunk-error "TOOL_CALL_CHUNK needs toolCallName on the first chunk of ~a" id))
        (setf out (append out (%close-tool expander) (%close-reasoning expander)))
        (setf out (append out (list (make-tool-call-start-event
                                     :tool-call-id id
                                     :tool-call-name name
                                     :parent-message-id
                                     (event-field event 'parent-message-id))))))
      (setf (expander-tool-id expander) id))
    (let ((delta (event-field event 'delta)))
      (when (and delta (plusp (length delta)))
        (setf out (append out (list (make-tool-call-args-event
                                     :tool-call-id id :delta delta))))))
    out))

(defmethod expand-chunk ((expander chunk-expander)
                         (event reasoning-message-chunk-event))
  ;; Spec: an empty delta closes the reasoning message, as does any following
  ;; non-reasoning event (handled by the default method above).
  (let* ((id (or (event-field event 'message-id) (expander-reasoning-id expander)))
         (delta (event-field event 'delta))
         (out '()))
    (unless id
      (%chunk-error "REASONING_MESSAGE_CHUNK needs messageId on the first chunk"))
    (when (and delta (zerop (length delta)) (equal id (expander-reasoning-id expander)))
      (return-from expand-chunk (%close-reasoning expander)))
    (unless (equal id (expander-reasoning-id expander))
      (setf out (append out (%close-reasoning expander)))
      (setf out (append out (list (make-reasoning-message-start-event
                                   :message-id id))))
      (setf (expander-reasoning-id expander) id))
    (when (and delta (plusp (length delta)))
      (setf out (append out (list (make-reasoning-message-content-event
                                   :message-id id :delta delta)))))
    out))

;;; An explicit START supersedes an open chunk sequence of the same family.

(macrolet ((supersedes (class closer)
             `(defmethod expand-chunk ((expander chunk-expander) (event ,class))
                (append (,closer expander) (%close-reasoning expander) (list event)))))
  (supersedes text-message-start-event %close-text)
  (supersedes tool-call-start-event %close-tool))

;;; Reasoning-family events leave an open reasoning message alone; only the
;;; explicit REASONING_MESSAGE_END or a non-reasoning event closes it.

(macrolet ((keeps-reasoning-open (&rest classes)
             `(progn
                ,@(mapcar (lambda (class)
                            `(defmethod expand-chunk ((expander chunk-expander)
                                                      (event ,class))
                               (list event)))
                          classes))))
  (keeps-reasoning-open reasoning-start-event
                        reasoning-end-event
                        reasoning-message-start-event
                        reasoning-message-content-event
                        reasoning-message-end-event
                        reasoning-encrypted-value-event))

(defun finish-chunks (expander)
  "Close every sequence still open at end of stream."
  (append (%close-text expander)
          (%close-tool expander)
          (%close-reasoning expander)))

(defun expand-ag-ui-chunks (events)
  "Expand a complete sequence of EVENTS, closing open sequences at the end."
  (let ((expander (make-chunk-expander)))
    (append (loop for event in events append (expand-chunk expander event))
            (finish-chunks expander))))
