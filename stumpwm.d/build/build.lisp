(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun library-translation (input dd)
   (declare (ignore dd))
   (merge-pathnames
     (make-pathname :directory
                    (list* :relative
                           "lisp-dylibs"
                           (cdr
                             (pathname-directory
                               (parse-namestring
                                 input))))
                    :defaults input)
     (user-homedir-pathname)))

 (defun is-library ()
   (make-pathname :directory (list :absolute :wild-inferiors)
                  :name :wild
                  :type "so"
                  :version :wild))

 (asdf:initialize-output-translations
   `(:output-translations
      :inherit-configuration
      (,(is-library) (:function library-translation)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :xembed)
  (ql:quickload :stumpwm)
  (ql:quickload :serapeum)
  (ql:quickload :clim-debugger)
  (ql:quickload :mpd-remote)
  (ql:quickload :ubiquitous) 
  (ql:quickload :swank)
  (let ((*features* (cons :devtime *features*)))
    (compile-file "commands")  
    (load "commands")  
    (compile-file "~/.stumpwmrc")  
    (load "~/.stumpwmrc"))

  (gc :full t))

(defun toplevel ()
  (let ((*debugger-hook* #'clim-debugger:debugger))
    (stumpwm:stumpwm)))

(save-lisp-and-die (truename "~/bin/stumpwm")
                   :executable t
                   :toplevel #'toplevel
                   :root-structures '(stumpwm:stumpwm)
                   :compression t)
