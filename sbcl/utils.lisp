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
    (unless (probe-file target)
      (legit:clone git-url
                   (ensure-directories-exist target)))
    (directory (merge-pathnames (make-pathname :directory (list :relative :wild-inferiors)
                                               :name :wild
                                               :type "asd")
                                target))))

(defvar *fwoar/reset-cache* nil)
(defmacro $caching (&body body)
  (alexandria:with-gensyms (cache)
    `(let ((,cache (load-time-value (vector nil nil))))
       (cond ((and (elt ,cache 1)
                   (not *fwoar/reset-cache*))
              (elt ,cache 0))
             (t
              (prog1 (setf (elt ,cache 0)
                           (progn ,@body))
                (setf (elt ,cache 1) t
                      *fwoar/reset-cache* nil)))))))
(defun gf-repos/raw ()
  ($caching
    (trivial-ssh:with-connection
        (c "git.fiddlerwoaroof.com"
         (trivial-ssh:key "git" "Users/edwlan/.ssh/id_ed25519"))
        (libssh2:with-execute (s c "info")
          (let ((libssh2:*channel-read-zero-as-eof* t))
            (loop for line = (read-line s nil)
                  while line
                  when (serapeum:string-prefix-p " " line)
                    collect line))))))

(defun gf-repos (&optional (pattern nil pattern-p))
  (funcall (if pattern-p
               (data-lens:include (data-lens:on (data-lens:regex-match pattern)
                                                'car))
               #'identity)
           (mapcar (data-lens:• (lambda (it)
                                  (coerce it 'list))
                                #'reverse
                                (lambda (it)
                                  (data-lens.lenses:over (data-lens.lenses:make-list-lens 0)
                                                         (lambda (it) (fwoar.string-utils:split #\space it))
                                                         it))
                                (lambda (it) (fwoar.string-utils:split #\tab it)))
                   (gf-repos/raw))))

(defun gf (coordinate)
  (let ((git-url (format nil "git@git.fiddlerwoaroof.com:~a.git" coordinate))
        (target (:printv
                 (merge-pathnames (:printv
                                   (uiop:parse-unix-namestring coordinate
                                                               :ensure-directory t))
                                  (merge-pathnames (make-pathname :directory
                                                                  (list :relative
                                                                        "git_repos"
                                                                        "git.fiddlerwoaroof.com"))
                                                   (user-homedir-pathname))))))
    (unless (probe-file target)
      (legit:clone git-url
                   (ensure-directories-exist target)))
    (directory (merge-pathnames (make-pathname :directory (list :relative :wild-inferiors)
                                               :name :wild
                                               :type "asd")
                                target))))
