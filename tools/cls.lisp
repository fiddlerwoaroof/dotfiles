#+fw.dump
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :sb-posix)
  (require :uiop))

#+fw.dump
(ql:quickload '(:net.didierverna.clon :data-lens :yason))

(defpackage :fwoar.cls
  (:use :cl)
  (:export ))
(in-package :fwoar.cls)

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

(defun directoryp (path-stat)
  (sb-posix:s-isdir (sb-posix:stat-mode path-stat)))

(defun list-directory (path)
  (handler-case (progn (let ((stat (sb-posix:lstat path)))
                         (if (directoryp stat)
                             (values (map-directory-entries path
                                                            (data-lens:juxt (constantly :name) 'sb-posix:dirent-name
                                                                            (constantly :inode) 'sb-posix:dirent-ino))
                                     :directory)
                             (values (handle-non-directory path)
                                     (let ((mode (sb-posix:stat-mode stat)))
                                       (cond
                                         ((sb-posix:s-isreg mode) :regular-file)
                                         ((sb-posix:s-isblk mode) :block-device)
                                         ((sb-posix:s-ischr mode) :character-device)
                                         ((sb-posix:s-issock mode) :socket)
                                         ((sb-posix:s-islnk mode) :symlink)
                                         ((sb-posix:s-isfifo mode) :fifo)
                                         (t :other)))))))
    (error (c) (format *error-output* "~a" c))))

(defun main-ld (paths)
  (loop for path in paths
        do
           (yason:with-output (*standard-output*)
             (let ((yason:*symbol-key-encoder* 'yason:encode-symbol-as-lowercase)
                   (yason:*symbol-encoder* 'yason:encode-symbol-as-lowercase))
               (yason:with-object ()
                 (yason:encode-object-element "path" path)
                 (multiple-value-bind (data type) (list-directory path)
                   (yason:encode-object-element "type" type)
                   (when (eql :directory type)
                     (yason:with-object-element ("children")
                       (yason:with-array ()
                         (loop for it in data
                               do (yason:encode-array-element (alexandria:plist-hash-table it))))))))
               (terpri *standard-output*)))))

(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context)
         (hashes '(:md5 :sha256)))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))


          (t
           (main-ld (net.didierverna.clon:remainder :context context))
           (terpri)))))

(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*))
  (net.didierverna.clon:dump "cls" main))
