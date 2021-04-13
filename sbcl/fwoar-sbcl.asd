(asdf:defsystem fwoar-sbcl
  :depends-on ((:require :sb-aclrepl)
               (:require :sb-introspect)
               :alexandria
               :cffi
               :fwoar-lisputils
               :legit
               :linedit
               :osicat
               :serapeum)
  :components ((:file "utils")))
