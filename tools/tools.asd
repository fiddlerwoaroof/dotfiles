;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :tools
  :description "Tool for generating colored text"
  :author "Ed L <el-l@elangley.org>"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ())

(defclass asdf-user::fw-tool-system (asdf:system)
  ())
(defmethod asdf:perform :before ((o program-op) (c fw-tool-system))
  (flet ((package-name-for-system ()
           (let* ((primary-name (asdf:primary-system-name c)))
             (format nil "fwoar.~a"
                     (subseq (asdf:component-name c)
                             (1+ (length primary-name)))))))
    (uiop:symbol-call (string-upcase (package-name-for-system))
                      :prepare-dump)))
(defmethod asdf:output-files ((o program-op) (c fw-tool-system))
  (let* ((exe-name (asdf/system:component-build-pathname c))
         (result (merge-pathnames exe-name
                                  (user-homedir-pathname))))
    (format *error-output* "NOTICE ME: output to ~s" result)
    (values (list result)
            t)))

(defsystem :tools/zenburn
  :description "Tool for generating colored text"
  :author "Ed L <el-l@elangley.org>"
  :license "MIT"
  ;; :defsystem-depends-on (:tool-system)
  :class fw-tool-system
  :build-pathname "zenburn"
  :entry-point "fwoar.zenburn:main"
  :depends-on (#:alexandria
               #:dufy
               #:net.didierverna.clon
               #:serapeum
               #:uiop)
  :serial t
  :components ((:file "zenburn")))

(defsystem :tools/cls
  :description "list files as json"
  :author "Ed L <el-l@elangley.org>"
  :license "MIT"
  :class fw-tool-system
  :build-pathname "cls"
  :entry-point "fwoar.cls:main"
  :depends-on ((:require :sb-posix)
               (:require :uiop)
               #:alexandria
               #:data-lens
               #:local-time
               #:net.didierverna.clon
               #:yason)
  :serial t
  :components ((:file "cls")))

(defsystem :tools/git-pick-patch
  :description "list files as json"
  :author "Ed L <el-l@elangley.org>"
  :license "MIT"
  :build-pathname "git-pick-patch"
  :entry-point "git-pick-patch:main"
  :depends-on (#:alexandria
               #:serapeum
               #:cl-ppcre)
  :output-files (program-op (o c)
                            (let* ((exe-name (asdf/system:component-build-pathname c))
                                   (result (merge-pathnames exe-name
                                                            (user-homedir-pathname))))
                              (format *error-output* "NOTICE ME: output to ~s" result)
                              (values (list result)
                                      t)))
  :serial t
  :components ((:file "git-pick-patch")))
