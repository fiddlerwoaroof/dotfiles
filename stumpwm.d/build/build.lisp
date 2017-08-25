(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

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
