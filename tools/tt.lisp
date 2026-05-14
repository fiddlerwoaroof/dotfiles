(defpackage :fwoar.tt
  (:use :cl )
  (:export :main :prepare-dump :dump))
(in-package :fwoar.tt)

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "template-name" :make-default nil)
    (flag :short-name "h" :long-name "help")))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context)
         (remainder (net.didierverna.clon:remainder :context context)))
    (cond ((or (null remainder)
               (not (null (cdr remainder)))
               (net.didierverna.clon:getopt :context context
                                            :long-name "help"))
           (net.didierverna.clon:help))


          (t
           (mustache:render (truename (car remainder))
                            (yason:parse *standard-input* :object-as :alist)
                            *standard-output*)
           (fresh-line *standard-output*)))))

(defun prepare-dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.main (remove :fw.dump *features*))))

(defun dump ()
  (prepare-dump)
  (net.didierverna.clon:dump "tt" main))
