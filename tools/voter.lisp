#+fw.dump
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :asdf))

#+fw.dump
(progn
  (asdf:load-asd
   (asdf:system-relative-pathname :net.didierverna.clon "termio/net.didierverna.clon.termio.asd"))
  (require :uiop))

#+fw.dump
(ql:quickload '(:net.didierverna.clon :yason :fwoar-lisputils))

(defpackage :fwoar.voter
  (:use :cl)
  (:export ))
(in-package :fwoar.voter)

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis ()
    (flag :short-name "h" :long-name "help")))

(defun get-json ()
  (loop with running = t
        while running
        append (handler-case (yason:parse *standard-input*
                                          :object-as :alist
                                          :json-booleans-as-symbols t)
                 (end-of-file ()
                   (setf running nil)))))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))
          (t
           (pprint
            (remove-if
             (lambda (it) (< it 2))
             (fwoar.counter::extract-counts
              (fwoar.counter::count-sequence
               (mapcar 'car
                       (remove 'yason:false
                               (sort (get-json)
                                     'string<
                                     :key 'car)
                               :key 'cdr))
               :test 'equal))
             :key 'cdr))
           (fresh-line)))))

(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*))
  (net.didierverna.clon:dump "voter" main))

(defvar *i*)
(defun refresh-i ()
  (setf *i* (make-string-input-stream
             "{\"nsfw\": true,\"sfw\": false,\"nudity\": true,\"woman\": true,\"topless\": true,\"lingerie\": false,\"panties\": false,\"bottom\": false,\"barefoot\": false,\"towel\": true,\"naked\": true,\"body\": true,\"interior\": true,\"bathroom\": true,\"shower\": true,\"hair\": true,\"makeup\": false,\"earrings\": false,\"jewelry\": false,\"tattoo\": false}
{\"sfw\": true,\"nsfw\": false,\"woman\": true,\"topless\": true,\"nudity\": false,\"clothing\": true,\"lingerie\": false,\"bathrobe\": true,\"towel\": true}
{\"nsfw\": true,\"sfw\": false,\"nudity\": true,\"woman\": true,\"topless\": true,\"lingerie\": false,\"panties\": false,\"bottom\": false,\"clothing\": false}")))
