;;; to run, do something like:
;;;  <path_to_lispworks>/lispworks-7-1-0-amd64-darwin -build ~/git_repos/dotfiles/lispworks/saveimg.lisp
(in-package "CL-USER")
(load-all-patches)
(let ((out-name (format nil "~~/bin/lw-console-~a" 
                        (lisp-implementation-version))))
  (save-image out-name 
              :console t
              :multiprocessing t
              :environment nil))
