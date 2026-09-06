;;; Alias well-known type descriptors so vendored ag_ui protos can
;;; :import "google/protobuf/struct.proto" without a second copy of the file.
;;; cl-protobufs WKT registers #P"struct.proto"; the official import path is longer.

(cl:in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((name (find-symbol "STRUCT" :cl-protobufs.google.protobuf))
         (desc (and name (cl-protobufs:find-file-descriptor name))))
    (unless desc
      (setf desc (cl-protobufs:find-file-descriptor #P"struct.proto")))
    (unless desc
      (error "cl-protobufs WKT Value/Struct is not loaded"))
    (setf (gethash #P"google/protobuf/struct.proto"
                   cl-protobufs.implementation::*file-descriptors*)
          desc)))
