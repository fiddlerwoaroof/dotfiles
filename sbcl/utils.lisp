(in-package :cl-user)

(defun load-project-asds (name)
  (mapcar 'asdf:load-asd
          (directory (make-pathname :host "PROJECTS"
                                    :directory (list :absolute (string-upcase name))
                                    :name :wild
                                    :type "ASD"))))

#+(or)
(mapcar 'asdf:load-asd
        (remove #\. (directory "*.asd")
                :test #'eql
                :key (data-lens:• (data-lens:element 0)
                                  #'pathname-name)))

(defun gh (coordinate)
  (let ((git-url (format nil "git@github.com:~a.git" coordinate))
        (target (:printv
                 (merge-pathnames (:printv
                                   (uiop:parse-unix-namestring coordinate
                                                               :ensure-directory t))
                                  (merge-pathnames (make-pathname :directory
                                                                  (list :relative
                                                                        "git_repos"
                                                                        "github.com"))
                                                   (user-homedir-pathname))))))
    (legit:clone git-url
                 (ensure-directories-exist target))))
