#+(or)
(eval-when (:load-toplevel :execute)
  (format t "~&Loading dependencies...")
  (ql:quickload '(:alexandria :serapeum :fwoar-lisputils :drakma
                  :flexi-streams :osicat :yason :trivia)))

#+(or)
(eval-when (:load-toplevel :execute)
  (format t "~&Loading dependencies...")
  (osicat-posix:setenv "CC" "/usr/bin/gcc")
  (ql:quickload :net.didierverna.clon))

(defpackage :fwoar.ddns-updater
  (:use :cl :alexandria :serapeum)
  (:export :update-domains :main))
(in-package :fwoar.ddns-updater)

(defparameter *update-url* "https://api.1984.is/1.0/freedns/?apikey=~a&domain=~a&ip=")

(defvar *http-stream* nil)

(defmacro with ((var val) &body body)
  "A stripped down let for binding a single name"
  `(let ((,var ,val))
     ,@body))

(defun update-domain (domain api-key)
  (let* ((url (format nil *update-url* api-key domain))
	       (drakma:*text-content-types* (acons "application" "json" drakma:*text-content-types*)))
    ;; todo: we probably want to read the stream in, because yason isn't completely robust against early termination
    (multiple-value-bind (data _ __ ___ ____ stream) (drakma:http-request url :close nil :stream *http-stream*)
      (declare (ignore _ __ ___ ____))
      (unless *http-stream*
	      (setf *http-stream* stream))
      (with (result (yason:parse data))
	      (values result
		            (gethash "ok" result)
		            (gethash "msg" result))))))

(defun update-domains (domains api-key)
  (mapcar (op (with-simple-restart (continue "Skip ~a" _1)
		            (format t "~&Updating ~a...~%" _1)
		            (prog1 (multiple-value-list (update-domain _1 api-key))
		              (sleep 1))))
	        domains))

(defvar *api-key*)
(defvar *domains*)

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis ()
    (flag :short-name "h" :long-name "help")))

(defun main ()
  (pushnew `("SYS:SITE;**;*.*.*" ,(merge-pathnames
                                   (make-pathname :directory (list
                                                              :relative ".sbcl" "site"
                                                              :wild-inferiors)
                                                  :name :wild
                                                  :type :wild)
                                   (user-homedir-pathname)))
           (logical-pathname-translations "SYS")
           :test #'equal)

  (mapcar (lambda (_)
            (load-logical-pathname-translations (pathname-name _)))
          (directory #p"SYS:SITE;*.translations"))


  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context))

    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))
          (t
           (trivia:match (uiop:read-file-form "CONFIG:1984.sexp")
             ((trivia:plist :api-key *api-key*
                            :domains *domains*)
              (unwind-protect (update-domains *domains*
                                              *api-key*)
                (when *http-stream*
                  (finish-output *http-stream*)
                  (close *http-stream*)))))))))

(defun prepare-dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.main (remove :fw.dump *features*))))
