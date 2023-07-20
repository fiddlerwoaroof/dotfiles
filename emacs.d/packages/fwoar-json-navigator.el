;;; fwoar-json-navigator.el --- more functional utilities for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Version: 0.0.1
;; Keywords: json,navigator
;; URL: https://fwoar.co
;; Package-Requires: (json-mode fwoar-functional-utils)

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

;; simple attempts to navigate json

;;; Code:
(require 'fwoar-functional-utils)
(require 'json-mode)

(defvar-local fwoar/json-nav--data nil)
(defvar-local fwoar/json-nav--path nil)
(defvar-local fwoar/json-nav--prev-buffer nil)
(defvar-local fwoar/json-nav--start-buffer nil)

(defun fwoar/browse-json-response (url)
  (interactive "Murl? ")
  (let ((json (url-retrieve-synchronously url)))
    (comment
     (buffer (generate-new-buffer
              (format "*API result: %s*" url))))
    (with-current-buffer json
      (when-let* ((buffer-name (buffer-name))
                  ((s-prefix-p " " buffer-name)))
        (rename-buffer (subseq buffer-name 1)))
      (json-mode)
      (goto-char (point-min))
      (kill-paragraph 1)
      (kill-line)
      (json-pretty-print-buffer)
      (comment
       (insert (with-current-buffer json
                 (buffer-string)))))
    (switch-to-buffer json)))

(defun fwoar/json--ensure-data ()
  (save-excursion
    (goto-char (point-min))
    (setq-local fwoar/json-nav--data (json-parse-buffer :null-object nil)))
  (values))

(defun fwoar/json-nav--pierce-vectors (fun it)
  (cl-typecase it
    (vector (map 'vector
                 (lambda (next)
                   (fwoar/json-nav--pierce-vectors fun next))
                 it))
    (t (funcall fun it))))

(defun fwoar/json-nav--get-path (data path)
  (cl-loop with cur = data for key in path
           do
           (setf cur
                 (cl-etypecase cur
                   (vector
                    (fwoar/json-nav--pierce-vectors (fwoar/key key)
                                                    cur))
                   (hash-table
                    (funcall (fwoar/key key)
                             cur))
                   (null ())))
           finally (return cur)))

(cl-defmacro fwoar/json-nav--with-collector ((c) &body body)
  (declare (indent 1))
  (let ((v (gensym "v")))
    `(let ((,v ()))
       (cl-flet ((,c (it) (push it ,v)))
         ,@body
         (nreverse ,v)))))

(defun fwoar/json-nav--get-keys ()
  (fwoar/json--ensure-data)
  (let ((data (fwoar/json-nav--get-path fwoar/json-nav--data
                                        (reverse fwoar/json-nav--path))))
    (sort (cl-etypecase data
            (hash-table (hash-table-keys data))
            (vector (remove-duplicates (sort (fwoar/json-nav--with-collector (c)
                                               (fwoar/json-nav--pierce-vectors
                                                (lambda (next)
                                                  (when next
                                                    (map nil #'c
                                                         (hash-table-keys next))))
                                                data))
                                             'string<)
                                       :test 'equal)))
          'string<)))

(defun fwoar/dive (s)
  (interactive (list (completing-read "key? "
                                      (fwoar/json-nav--get-keys))))
  (fwoar/json--ensure-data)
  (let* ((path (cons s fwoar/json-nav--path))
         (data fwoar/json-nav--data)
         (last-buffer (buffer-name)))
    (with-current-buffer (switch-to-buffer-other-window
                          (format "*test-buffer: %s*"
                                  (s-join "/" (reverse path))))
      (json-mode)
      (setq-local fwoar/json-nav--data data
                  fwoar/json-nav--path path
                  fwoar/json-nav--prev-buffer last-buffer)
      (setf (buffer-string)
            (json-serialize (fwoar/json-nav--get-path fwoar/json-nav--data
                                                      (reverse path))
                            :null-object nil))
      (json-pretty-print-buffer)
      (goto-char (point-min))))
  (goto-char (point-min)))

(defun fwoar/return ()
  (interactive)
  (when fwoar/json-nav--prev-buffer
    (let ((old-buffer (current-buffer))
          (prev-buffer fwoar/json-nav--prev-buffer))
      (switch-to-buffer-other-window fwoar/json-nav--prev-buffer)
      (unless fwoar/json-nav--prev-buffer
        (with-current-buffer prev-buffer
          (delete-other-windows)))
      (kill-buffer old-buffer)))
  (point-min))

(provide 'fwoar-json-navigator)
;;; fwoar-json-navigator.el ends here
