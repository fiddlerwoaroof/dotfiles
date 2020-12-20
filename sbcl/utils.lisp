(in-package :cl-user)

(defun load-project-asds (name)
  (mapcar 'asdf:load-asd
          (directory (make-pathname :host "PROJECTS"
                                    :directory (list :absolute (string-upcase name))
                                    :name :wild
                                    :type "ASD"))))
