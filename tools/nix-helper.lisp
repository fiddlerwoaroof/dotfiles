#!/usr/bin/env -S sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload '(:yason :uiop :cl-ppcre :serapeum
                :data-lens/transducers :fwoar-lisputils))

(defpackage :fwoar.nix-helper
  (:use :cl )
  (:export ))
(in-package :fwoar.nix-helper)

(defun replace-regexes (from to str)
  (assert (= (length from) (length to)))
  (if (null from)
      str
      (replace-regexes
       (rest from)
       (rest to)
       (cl-ppcre:regex-replace-all (first from) str (first to)))))

(defun nixify-symbol (string)
  (flet ((fix-special-chars (str)
           (replace-regexes '("[_]" "[+]$" "[+][/]" "[+]" "[.]" "[/]")
                            '("__" "_plus" "_plus/" "_plus_" "_dot_" "_slash_")
                            str)))
    (if (ppcre:scan "^[0-9]" string)
        (serapeum:concat "_" (fix-special-chars string))
        (fix-special-chars string))))

(defun find-subsystems (system)
  (funcall (data-lens:include
            (data-lens:regex-match
             (string-downcase system)))
           (asdf:registered-systems)))


(defun eliminate-requires (deps)
  (labels ((handle-dep (dep)
             (typecase dep
               (cons (case (car dep)
                       (:feature (when (uiop:featurep (cadr dep))
                                   (handle-dep (caddr dep))))
                       (t nil)))
               (string (list dep)))))
    (mapcan #'handle-dep
            deps)))

(defun transitive-dependencies (system)
  (loop with stack = (list system)
        for next = (pop stack)
        for old-deps = (list system) then (append old-deps new)
        for next-deps = (asdf:system-depends-on (asdf:find-system next))
        for new = (eliminate-requires (set-difference next-deps old-deps :test #'equal))
        do (setf stack (append stack new))
        while stack
        append new))

(defun get-dependencies (system)
  (list system
        (coerce (mapcar #'nixify-symbol
                        (clean-deps (transitive-dependencies system)))
                'vector)))

(defun serialize-dependencies (s dependency-map)
  (yason:with-output (s :indent t)
    (yason:with-object ()
      (loop for (system dependencies) in dependency-map
            do (yason:with-object-element (system)
                 (yason:encode dependencies))))))

(defun clean-deps (deps)
  (remove-duplicates
   (remove-if (lambda (it)
                (or (serapeum:string-prefix-p "sb-" (string-downcase it))
                    (member it '("uiop"
                                 "sb-posix")
                            :test #'equal)))
              (mapcar (lambda (it)
                        (first (fwoar.string-utils:partition #\/ it)))
                      deps))
   :test #'equal))

(defun serialize-primary-and-secondary-system-deps (s system)
  (serialize-dependencies s
                          (mapcar #'get-dependencies
                                  (find-subsystems system))))

(defun doit (output-fn system)
  (alexandria:with-output-to-file (s output-fn :if-exists :supersede)
    (serialize-primary-and-secondary-system-deps s system)))

(progn
  (format t "NOTICE ME: ~s~%" (truename (caddr (uiop:command-line-arguments))))
  (asdf:load-asd (truename (caddr (uiop:command-line-arguments))))
  (format t "NOTICE ME: ~s~%" (asdf:find-system (cadr (uiop:command-line-arguments))))
  ;; (ql:quickload (cadr (uiop:command-line-arguments)))
  (doit (car (uiop:command-line-arguments))
        (cadr (uiop:command-line-arguments)))
  )
