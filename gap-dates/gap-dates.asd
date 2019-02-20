;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :gap-dates 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:alexandria
               #:fwoar-lisputils)
  :serial t
  :components ((:file "gap-dates")))
