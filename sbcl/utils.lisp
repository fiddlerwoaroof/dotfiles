(in-package :cl-user)

(define-condition repl-error (error)
  ())
(define-condition no-such-directory (repl-error)
  ((%attempted-directory :reader attempted-directory :initarg :ad))
  (:report (lambda (condition stream)
             (format stream "No such directory: ~a" (attempted-directory condition)))))

(defun read-evaluated-form (&optional (prompt-control nil promptp)
                            &rest prompt-args)
  (apply #'format *query-io*
         (if promptp prompt-control "~&Enter a form to be evaluated: ")
         prompt-args)
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun cd (new)
  (flet ((%cd (new-directory)
           (if (probe-file new-directory)
               (return-from cd
                 (setf *default-pathname-defaults* (truename new-directory)))
               (cerror "Try again" 'no-such-directory :ad new-directory))))
    (fw.lu:retry-once (is-retry)
      (let ((new-directory (make-pathname :directory (append (pathname-directory *default-pathname-defaults*)
                                                             (list new))
                                          :defaults *default-pathname-defaults*)))
        (restart-case (%cd new-directory)
          (use-value (value)
            :test (lambda (_) _ is-retry)
            :report (lambda (stream)
                      (format stream "Use specified value."))
            :interactive read-evaluated-form
            (setf new value)))))))

(defun ls (&optional pattern)
  (if pattern
      (directory pattern)
      (directory "*.*")))

(defun rm (file)
  (unless (listp file)
    (setf file (list file)))
  (mapcar #'delete-file file))

(defun --parse-path (it)
  (let ((parts (fwoar.string-utils:split #\/ it)))
    (values (elt parts (1- (length parts)))
            (coerce (subseq parts 0 (1- (length parts)))
                    'list))))

(defun load-project-asds (name)
  (multiple-value-bind (proj-name proj-sub) (--parse-path name)
    (mapcar 'asdf:load-asd
            (directory (make-pathname :host "PROJECTS"
                                      :directory (list :absolute (string-upcase proj-name))
                                      :name :wild
                                      :type "ASD")))))

#+(or)
(mapcar 'asdf:load-asd
        (remove #\. (directory "*.asd")
                :test #'eql
                :key (data-lens:• (data-lens:element 0)
                                  #'pathname-name)))

(defun gh-repo-root (coordinate)
  (let ((git-url (format nil "git@github.com:~a.git" coordinate)))
    (merge-pathnames (uiop:parse-unix-namestring coordinate
                                                   :ensure-directory t)
                      (merge-pathnames (make-pathname :directory
                                                      (list :relative
                                                            "git_repos"
                                                            "github.com"))
                                       (user-homedir-pathname)))))

(defun gh-coordinate (coordinate)
  (format nil "git@github.com:~a.git" coordinate))

(defun gh-dir (coordinate)
  (uiop:nest (merge-pathnames (uiop:parse-unix-namestring coordinate :ensure-directory t))
             (merge-pathnames (make-pathname :directory (list :relative "git_repos" "github.com")))
             (user-homedir-pathname)))

(defun gh (coordinate)
  (let ((git-url (gh-coordinate coordinate))
        (target (:printv (gh-dir coordinate))))
    (unless (probe-file target)
      (legit:clone git-url
                   (ensure-directories-exist target)))
    (directory (merge-pathnames (make-pathname :directory (list :relative :wild-inferiors)
                                               :name :wild
                                               :type "asd")
                                target))))

(defun gl (coordinate)
  (let ((git-url (format nil "git@gitlab.com:~a.git" coordinate))
        (target (:printv
                 (merge-pathnames (:printv
                                   (uiop:parse-unix-namestring coordinate
                                                               :ensure-directory t))
                                  (merge-pathnames (make-pathname :directory
                                                                  (list :relative
                                                                        "git_repos"
                                                                        "gitlab.com"))
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
         (trivial-ssh:key "git" (namestring (merge-pathnames ".ssh/id_ed25519"
                                                             (user-homedir-pathname)))))
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

(defun lp (git-url)
  (let* ((uri (if (and (not (net.uri:uri-p git-url))
                       (find #\@ git-url))
                  (puri:parse-uri git-url)
                  git-url))
         (target (merge-pathnames (make-pathname :directory
                                                 (list :relative
                                                       "local-projects"
                                                       (etypecase uri
                                                         (puri:uri (car (last (puri:uri-parsed-path uri))))
                                                         (string (subseq uri
                                                                         (or (1+
                                                                              (position #\/ (string-right-trim "/" uri)
                                                                                        :from-end t))
                                                                             0))))))
                                  ql:*quicklisp-home*)))
    (legit:clone (etypecase git-url
                   (puri:uri (puri:render-uri git-url nil))
                   (string git-url))
                 (namestring target))))


(defclass fw-define-op (asdf:define-op)
  ((%systems-before :reader systems-before :initform (asdf:registered-systems))
   (%new-systems :initarg :new-systems :accessor new-systems)))
(defmethod asdf:operate :after ((o fw-define-op) (c asdf:system) &key)
  (setf (new-systems o) (set-difference (asdf:registered-systems)
                                        (slot-value o '%systems-before)
                                        :test 'equal)))
(defun load-asd (pathname &key name)
  "Load system definitions from PATHNAME.
NAME if supplied is the name of a system expected to be defined in that file.

Do NOT try to load a .asd file directly with CL:LOAD. Always use ASDF:LOAD-ASD."
  (asdf/session:with-asdf-session ()
    ;; TODO: use OPERATE, so we consult the cache and only load once per session.
    (flet ((do-it (o c) (asdf:operate o c)))
      (let ((primary-name (asdf:primary-system-name (or name (pathname-name pathname))))
            (operation (asdf:make-operation 'fw-define-op)))
        (uiop:if-let (system (asdf:registered-system primary-name))
          (progn
            ;; We already determine this to be obsolete ---
            ;; or should we move some tests from find-system to check for up-to-date-ness here?
            (setf (asdf/action:component-operation-time operation system) t
                  (asdf/system:definition-dependency-list system) nil
                  (asdf/system:definition-dependency-set system)
                  (uiop:list-to-hash-set nil))
            (do-it operation system))
          (let ((system (make-instance 'asdf/system:undefined-system
                                       :name primary-name :source-file pathname)))
            (asdf/system-registry:register-system system)
            (unwind-protect (do-it operation system)
              (when (typep system 'asdf/system:undefined-system)
                (asdf:clear-system system)))))))))

(export
 (defmacro vj ((op &rest args))
   `(values * (,op * ,@args))))

(export
 (defmacro wl ((op &rest args))
   `(values * (,op ,@args))))

(defun plot-stream (s &key
                        (xrange nil xrange-p)
                        (background "#494949")
                        (frame-color "#7fdf7f")
                        (line-color "#DCDCCC")
                        (lines nil))
  (let ((fn (format nil "/tmp/~a.svg" (gensym))))
    (uiop:run-program (format nil
                              "gnuplot -e \"~:[~*~;set xrange [~{~f~^:~}];~]set terminal svg font 'Alegreya,14' enhanced  background '~a'; set border lw 3 lc rgb '~a'; plot '< cat -' lt rgb '~a' notitle ~:[~;with lines~]\""
                              xrange-p xrange
                              background
                              line-color
                              frame-color
                              lines)

                      :input s
                      :output (parse-namestring fn))
    (swank::send-to-emacs (list :write-image fn " ")))
  (values))
