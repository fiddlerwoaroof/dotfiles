#+fw.dump
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :sb-posix)
  (require :uiop))

#+fw.dump
(ql:quickload '(:net.didierverna.clon :data-lens :yason :local-time))

(defpackage :fwoar.cls
  (:use :cl)
  (:export dump))
(in-package :fwoar.cls)

(defun format-stat-time (accessor stat)
  (local-time:format-timestring
   nil
   (local-time:unix-to-timestamp
    (funcall accessor
             stat))))

(defun map-directory-entries (dir cb)
  (let ((dp (sb-posix:opendir dir)))
    (unwind-protect (loop for dirent = (sb-posix:readdir dp)
                          until (sb-alien:null-alien dirent)
                          collect (funcall cb dirent))
      (sb-posix:closedir dp))))

(defun handle-non-directory (path)
  path)

(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "PATHS..." :make-default nil)
    (flag :short-name "h" :long-name "help")))

(defun stat-file-type (path-stat)
  (let ((mode (sb-posix:stat-mode path-stat)))
    (cond
      ((sb-posix:s-isdir mode))
      ((sb-posix:s-isreg mode) :regular-file)
      ((sb-posix:s-isblk mode) :block-device)
      ((sb-posix:s-ischr mode) :character-device)
      ((sb-posix:s-issock mode) :socket)
      ((sb-posix:s-islnk mode) :symlink)
      ((sb-posix:s-isfifo mode) :fifo)
      (t :other))))

(defun decode-permissions (mode)
  (let* ((user (ash (logand #o700 mode) -6))
         (group (ash (logand #o70 mode) -3))
         (other (logand #o7 mode)))
    (list (list :user
                (when (= #o4 (logand #o4 user))
                  :read)
                (when (= #o2 (logand #o2 user))
                  :write)
                (when (= 1 (logand #o1 user))
                  :execute))
          (list :group
                (when (= #o4 (logand #o4 group))
                  :read)
                (when (= #o2 (logand #o2 group))
                  :write)
                (when (= 1 (logand #o1 group))
                  :execute))
          (list :other
                (when (= #o4 (logand #o4 other))
                  :read)
                (when (= #o2 (logand #o2 other))
                  :write)
                (when (= 1 (logand #o1 other))
                  :execute)))))

(defun directoryp (path-stat)
  (sb-posix:s-isdir (sb-posix:stat-mode path-stat)))

(defun prepend-path-component (prefix)
  (let ((prefix (if (eql #\/
                         (elt prefix (1- (length prefix))))
                    (subseq prefix 0 (1- (length prefix)))
                    prefix)))
    (lambda (suffix)
      (format nil "~a/~a" prefix suffix))))

(defun list-directory (path stat)
  (handler-case (if (directoryp stat)
                    (values (map-directory-entries
                             path
                             (data-lens:juxt
                              (constantly :name) 'sb-posix:dirent-name
                              (constantly :path) (data-lens:∘
                                                  (prepend-path-component path)
                                                  'sb-posix:dirent-name)
                              (constantly :inode) 'sb-posix:dirent-ino))
                            :directory
                            (decode-permissions (sb-posix:stat-mode stat)))
                    (values (handle-non-directory path)
                            (stat-file-type stat)
                            (decode-permissions (sb-posix:stat-mode stat))))
    (error (c) (format *error-output* "~a (~a) ~a" path (type-of c) c))))

(defun name-or-dirname (pathname)
  (or (pathname-name pathname)
      (car (last (pathname-directory pathname)))))

(defun list-path (path)
  (yason:with-output (*standard-output*)
    (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
          (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase)
          (stat (sb-posix:lstat path)))
      (yason:with-object ()
        (yason:encode-object-element
         "name"
         (name-or-dirname (uiop:parse-unix-namestring path)))
        (yason:encode-object-element "path" path)
        (yason:encode-object-element "atime" (format-stat-time 'sb-posix:stat-atime
                                                               stat))
        (yason:encode-object-element "mtime" (format-stat-time 'sb-posix:stat-atime
                                                               stat))
        (yason:encode-object-element "ctime" (format-stat-time 'sb-posix:stat-atime
                                                               stat))
        (multiple-value-bind (data type permissions) (list-directory path stat)
          (yason:encode-object-element "type" type)
          (yason:encode-object-element "mode" (alexandria:alist-hash-table
                                               permissions))
          (when (eql :directory type)
            (yason:with-object-element ("children")
              (yason:with-array ()
                (loop for it in data
                      do (yason:encode-array-element
                          (alexandria:plist-hash-table it))))))))
      (terpri *standard-output*))))

(defun main-ld (paths)
  (loop for path in paths
        do
           (list-path path)))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))


          (t
           (let ((input (net.didierverna.clon:remainder :context context)))
             (if input
                 (main-ld input)
                 (loop for input = (read-line *standard-input*
                                              nil)
                       while input
                       for parsed = (coerce (gethash "path" (yason:parse input))
                                            'simple-string)
                       do (main-ld (list parsed)))))
           (fresh-line)))))

(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*))
  (net.didierverna.clon:dump "cls" main))
