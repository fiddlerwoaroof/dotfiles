#+fw.dump
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :uiop))

#+fw.dump
(ql:quickload '(:net.didierverna.clon :bit-smasher
                :ironclad :lparallel :serapeum))

(defpackage :fwoar.bloomutil
  (:use :cl)
  (:export ))
(in-package :fwoar.bloomutil)

(defun byte-array-to-integer (arr)
  (loop for cur across arr
        for sum = cur then (+ (ash sum 8) cur)
        finally (return sum)))

(defun file->idx (filter hash fn)
  (mod (byte-array-to-integer (ironclad:digest-file hash fn))
       (length filter)))

(defun bloom-file (filter hashes fn)
  (loop for hash in hashes
        do (setf (aref filter (file->idx filter hash fn)) 1))
  filter)

(defun parse-filter (filter)
  (bitsmash:hex->bits filter))

(defun serialize (filter)
  (bitsmash:bits->hex filter))

(defun has-file (filter hashes file)
  (loop with matches = 0
        for hash in hashes
        for count from 1
        do
           (when (= (elt filter (file->idx filter hash file))
                    1)
             (incf matches))
        finally (return (= matches count))))

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "FILTER FILES..." :make-default nil)
    (flag :short-name "h" :long-name "help")
    (flag :short-name "c" :long-name "check")
    (flag :long-name "coord")))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context)
         (lparallel:*kernel* (lparallel:make-kernel (serapeum:count-cpus)))
         (hashes '(:md5 :sha256)))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))
          ((net.didierverna.clon:getopt :context context
                                        :long-name "check")
           (destructuring-bind (filter file)
               (net.didierverna.clon:remainder :context context)
             (if (has-file (parse-filter filter)
                           hashes
                           file)
                 (princ "true")
                 (princ "false")))
           (terpri))
          ((net.didierverna.clon:getopt :context context
                                        :long-name "coord")
           (destructuring-bind (filter . files)
               (net.didierverna.clon:remainder :context context)
             (loop for file in files
                   do (format t "(~{~4,' d~^ ~} ~a)~%"
                              (loop for hash in hashes
                                    collect (file->idx filter hash file))
                              file))
             (terpri)))
          (t
           (destructuring-bind (filter . files)
               (net.didierverna.clon:remainder :context context)
             (let ((bits (parse-filter filter)))
               (format t "~v,1,0,'0@a"
                       (length filter)
                       (serialize
                        (lparallel:preduce 'bit-ior
                                           (lparallel:pmap 'list
                                                           (lambda (file)
                                                             (bloom-file (copy-seq bits)
                                                                         hashes
                                                                         file))
                                                           files)
                                           :initial-value (copy-seq bits))))))
           (terpri)))))

(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*))
  (net.didierverna.clon:dump "bloomutil" main))
