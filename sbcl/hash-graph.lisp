(defpackage :fwoar.hash-graph
  (:use :cl )
  (:export
   #:do-graph))
(in-package :fwoar.hash-graph)

(fw.lu:defclass+ hash-graph ()
  ((%edges :reader edges :initarg :edges)
   (%trim :reader trim :initarg :trim :initform nil)))

(defmethod cl-dot:graph-object-node ((graph hash-graph) (object string))
  (make-instance 'cl-dot:node
                 :id object))

(defmethod cl-dot:graph-object-points-to ((graph hash-graph) (object string))
  (let ((result (gethash object
                         (edges graph))))
    (when (trim graph)
      (setf result (remove-if-not (trim graph) result)))
    result))


(defun do-graph (hash start)
  (let* ((graph (hash-graph hash #+(or)(lambda (s) (serapeum:string-contains-p "/" s))))
         (dgraph (cl-dot:generate-graph-from-roots graph (list start)
                                                   '(:rankdir "LR")))
         (cl-dot:*dot-path* (namestring (truename "~/git_repos/dotfiles/scripts/prettydot"))))
    (cl-dot:dot-graph dgraph "test.svg" :format :svg)))
