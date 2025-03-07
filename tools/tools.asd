;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :tools/zenburn
  :description "Tool for generating colored text"
  :author "Ed L <el-l@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:net.didierverna.clon
               #:alexandria
               #:dufy)
  :serial t
  :components ((:file "zenburn")))
