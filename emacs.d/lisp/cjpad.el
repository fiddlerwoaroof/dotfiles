;;; cjpad.el --- Skeletons for interacting with cjpad

;; Copyright (C) 2017 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Keywords: cjpad
;; Version: 0.0.1

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

;; Put a description of the package here

;;; Code:

;; code goes here

(defun extract-pad-name (pad-url)
  (replace-regexp-in-string "^https?://cjpad.cj.com/\\([^/]+\\)/?$"
                            "\\1"
                            pad-url))

(defun get-pad-url (name)
  (format "http://cjpad.cj.com/ep/pad/export/%s/latest?format=txt"
          name))

(defun convert-to-export (pad-url)
  (format "http://cjpad.cj.com/ep/pad/export/%s/latest?format=txt"
          (extract-pad-name pad-url)))

(defvar *cjpad-exports* "~/cjpad-imports/")

(defun get-cjpad (pad-name)
  (interactive "M")
  (let ((file-name (format "%s%s" *cjpad-exports* pad-name))
        (pad-url (get-pad-url pad-name)))
    (with-current-buffer (url-retrieve-synchronously (get-pad-url pad-name))
      (write-region nil nil file-name))
    (find-file file-name)))

(defun import-cjpad (s e)
  (interactive "r")
  (message "=============================")
  (let* ((pad-url (buffer-substring s e)))
    (get-cjpad (extract-pad-name pad-url))
    (save-excursion
      (end-of-line)
      (insert " [" file-name "]")
      (message "foo")))
  (message "============================="))

(defun update-cjpad-file (fn)
  (interactive "F")
  (message fn)
  (let* ((pad-name (file-name-base fn))
         (pad-url (get-pad-url pad-name))
         (file-name (format "%s%s" *cjpad-exports* pad-name)))
    (with-current-buffer (url-retrieve-synchronously pad-url)
      (write-region nil nil file-name))))

(defun update-cjpad ()
  (interactive)
  (update-cjpad-file (buffer-file-name (current-buffer))))

(defun browse-cjpad ()
  (interactive)
  (browse-url (format "http://cjpad.cj.com/%s"
                      (file-name-base (buffer-file-name (current-buffer))))))

(provide 'cjpad)
