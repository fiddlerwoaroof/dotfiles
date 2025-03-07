;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :tools
  :description "Tool for generating colored text"
  :author "Ed L <el-l@elangley.org>"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ())

(defsystem :tools/zenburn
  :description "Tool for generating colored text"
  :author "Ed L <el-l@elangley.org>"
  :license "MIT"
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
  :depends-on ((:require :sb-posix)
               (:require :uiop)
               #:alexandria
               #:data-lens
               #:local-time
               #:net.didierverna.clon
               #:yason)
  :serial t
  :components ((:file "cls")))
