#!/home/edwlan/bin/sbcl --script

#-noscript
(load "~/quicklisp/setup.lisp")
#-noscript
(ql:quickload '(:lquery
                :fwoar-lisputils
                :alexandria
                :net.didierverna.clon))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :uiop))

(defpackage :fwoar.ct->sexp
  (:use :cl)
  (:local-nicknames (:clon :net.didierverna.clon))
  (:export ))
(in-package :fwoar.ct->sexp)

(defparameter *args*
  (uiop:command-line-arguments))

(defun extract-refs (work root)
  (lquery:$
   (inline root)
   "p[title]"
   (combine (attr "title")
            "a:first-child")
   (map-apply (lambda (ref sibling)
                (let* ((sibling (elt sibling 0))
                       (parent (plump:parent sibling))
                       (text (progn (plump:remove-child sibling)
                                    (plump:text parent))))
                  (list 'ref
                        work
                        (serapeum:string-join
                         (fwoar.string-utils:split " "
                                                   ref)
                         "")
                        text))))))

(defun serialize-refs (out-fn refs)
  (alexandria:with-output-to-file (s out-fn
                                     :if-exists :append
                                     :if-does-not-exist :create)
    (let ((*print-case* :downcase))
      (map nil
           (lambda (ref)
             (prin1 ref s)
             (fresh-line s))
           refs))))

(defun translate (in out)
  (let ((root (plump:parse in))
        (out-path (parse-namestring out)))
    (serialize-refs out
                    (extract-refs (alexandria:make-keyword
                                   (string-upcase (pathname-name out-path)))
                                  root))))

(defparameter *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "OUT FILES...")
    (flag :short-name "h" :long-name "help"
          :description "Get help")))

(defun main ()
  (net.didierverna.clon:make-context :synopsis *synopsis*)

  (let ((*package* (find-package :fwoar.ct->sexp)))
    (destructuring-bind (out . files) (net.didierverna.clon:remainder)
      (format *error-output*
              "OUT: ~s~%FILES: ~s~%"
              out files)
      (map nil
           (lambda (file)
             (format *error-output* "PROCESSING FILE: ~s~%" file)
             (translate (parse-namestring file)
                        out))
           files))))

#-noscript
(net.didierverna.clon:dump (merge-pathnames "bin/ct-sexp"
                                            (user-homedir-pathname))
                           main)
