(asdf:defsystem :fwoar-sbcl
  :depends-on ((:require :sb-aclrepl)
               (:require :sb-introspect)
               :alexandria
               :cffi
               :data-lens
               :fwoar-lisputils
               :legit
               :linedit
               :osicat
               :printv
               :puri
               :serapeum
               #+(or)
               :swank
               :trivial-ssh)
  :components ((:file "utils")))
