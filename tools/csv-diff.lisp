#+fw.dump
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :uiop))

#+fw.dump
(ql:quickload '(:net.didierverna.clon :alexandria :sqlite :cl-csv))

(defpackage :fwoar.csv-diff
  (:use :cl )
  (:export #:dump))
(in-package :fwoar.csv-diff)

(defun insert-list (conn table list)
  (sqlite:execute-non-query
   conn
   (format nil
           "insert into ~a")))

(defun sqlite-diff (table l1 table2 l2))

#+fw.dump
(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "OLD NEW" :make-default nil)
    (flag :short-name "h" :long-name "help")))

#+fw.dump
(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context)
         (foreground (net.didierverna.clon:getopt :context context
                                                  :long-name "fg"))
         (background (net.didierverna.clon:getopt :context context
                                                  :long-name "bg"))
         (remainder (net.didierverna.clon:remainder :context context))
         (css (net.didierverna.clon:getopt :context context
                                           :long-name "css"))
         (hsv (net.didierverna.clon:getopt :context context
                                           :long-name "hsv"))
         (html (net.didierverna.clon:getopt :context context
                                            :long-name "html")))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))
          ((and html css)
           (format *error-output* "Can't use HTML and CSS options together~%")
           (net.didierverna.clon:help))
          (css
           (let ((values (cdr (assoc css *color-alist*))))
             (format t "rgb(~{~d~^, ~})~%" values)))
          (html
           (html-color html t))
          (hsv
           (hsv-color hsv t))
          ((null remainder)
           (summary))
          ((or foreground background)
           (zenburn-text foreground background "~{~a~^ ~}" remainder))
          (t
           (net.didierverna.clon:help)))))

#+fw.dump
(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*)
        *print-case* :downcase)
  (net.didierverna.clon:dump "zenburn" main))
