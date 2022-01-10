;;; fwoar-functional-utils.el --- more functional utilities for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Version: 0.0.1
;; Keywords: fp,combinators
;; URL: https://fwoar.co

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some functional programming utilities

;;; Code:

(eval-when (compile load eval)
  (defvar *fwoar/namespaced-funs* ()))

(cl-defmacro fwoar/def-ns-fun (name (&rest args) &body body)
  (declare (indent defun))
  (let ((namespaced-sym (intern (format "fwoar/%s" name))))
    `(progn
       (cl-pushnew '(,name ,args ,namespaced-sym)
                   *fwoar/namespaced-funs*
                   :test 'equal)
       (cl-defun ,namespaced-sym ,args
         ,@body))))

(fwoar/def-ns-fun just-after (pred)
  (lexical-let ((state nil))
    (lambda (it)
      (cond
       (state it)
       ((funcall pred it) (setf state t) nil)))))

;;;###autoload
(cl-defmacro with-unaliased (&body body)
  `(flet ,(loop for (name raw-args namespaced) in *fwoar/namespaced-funs*
                for rest-arg = (cl-find-if (fwoar/just-after
                                            (lambda (it)
                                              (member it '(&rest &body))))
                                           raw-args)
                for args = (cl-remove-if (lambda (it)
                                           (or (eql it rest-arg)
                                               (and (symbolp it)
                                                    (= ?&
                                                       (elt
                                                        (symbol-name it)
                                                        0)))))
                                         raw-args)

                collect `(,name ,raw-args
                                (,@(if rest-arg
                                       `(apply ',namespaced)
                                     (list namespaced))
                                 ,@(mapcar (lambda (it)
                                             (if (listp it)
                                                 (car it)
                                               it))
                                           args)
                                 ,@(when rest-arg
                                     (list rest-arg)))))
     ,@body))

(cl-defmacro fwoar/def-combinator (name (seq &rest args) &body body)
  (declare (indent defun))
  `(fwoar/def-ns-fun ,name ,args
     (lambda (,seq)
       ,@body)))

(fwoar/def-ns-fun iota (count &optional (start 0))
  (cl-loop for x from start
           repeat count
           collect x))

(fwoar/def-ns-fun applying (f &rest pos-args)
  (lambda (list)
    (apply f (append pos-args list))))

(fwoar/def-ns-fun on (fun key-fun)
  (lambda (it)
    (funcall fun (funcall key-fun it))))

(fwoar/def-combinator over (list f &rest args)
  (mapcar (lambda (it)
            (apply f it args))
          list))

(fwoar/def-combinator filter (list f &rest args)
  (cl-remove-if-not (lambda (it)
                      (apply f it args))
                    list))

(fwoar/def-combinator zip-with (lists f)
  (apply 'cl-mapcar f lists))

(fwoar/def-ns-fun element (num)
  (lambda (it)
    (elt it num)))

(fwoar/def-ns-fun hash-lookup (ht)
  (lambda (key)
    (gethash key ht)))

(cl-defgeneric fwoar/extract-key (map key)
  (:method ((map hash-table) key)
           (gethash key map))
  (:method ((map list) key)
           (typecase (car map)
             (cons (cdr (cl-assoc key map :test 'equal)))
             (t (cl-loop for (a-key . value) on map by #'cddr
                         when (equal key a-key) do
                         (return (car value)))))))

(fwoar/def-ns-fun key (key)
  (lambda (map)
    (fwoar/extract-key map key)))

(fwoar/def-ns-fun keys (key &rest keys)
  (lambda (map)
    (loop for key in (cons key keys)
          for value = (fwoar/extract-key map key)
          finally (return value))))

(comment
 (fwoar/def-ns-fun regex-match (regex)
   (lambda (data)
     (cl-ppcre:scan-to-strings regex data))))

(fwoar/def-ns-fun include (pred)
  (lambda (seq)
    (cl-remove-if-not pred seq)))

(fwoar/def-ns-fun exclude (pred)
  (lambda (seq)
    (cl-remove-if pred seq)))

(fwoar/def-ns-fun pick (selector)
  (lambda (seq)
    (cl-map 'list selector seq)))

(fwoar/def-ns-fun slice (start &optional end)
  (lambda (it)
    (cl-subseq it start end)))

(fwoar/def-ns-fun juxt (fun1 &rest r)
  (lambda (&rest args)
    (list* (apply fun1 args)
           (mapcar (lambda (f)
                     (apply f args))
                   r))))

(defalias 'fwoar/• '-compose)

(provide 'fwoar-functional-utils)
;;; fwoar-functional-utils.el ends here
