;;; fwoar-pastebin.el --- Save htmlized version of buffer to a server -*- lexical-binding: t; tab-width: 8; -*-

;; Copyright (C) 2017 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
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

(defgroup fwoar-pastebin nil
  "Configuration for simple pastebin"
  :prefix "fwoar-pastebin-"
  :group 'application)

(defcustom fwoar-pastebin-tramp-url nil
  "A tramp-writable url to the pastebin"
  :group 'fwoar-pastebin
  :type 'string)

(defcustom fwoar-pastebin-web-url-pattern nil
  "An appropriate URL for viewing the uploaded files"
  :group 'fwoar-pastebin
  :type 'string)

;;;###autoload
(defun pastebin-buffer ()
  (interactive)
  (let* ((extension (file-name-extension (elt (split-string (buffer-name)
                                                            "<")
                                              0)))
         (htmlized-buffer (htmlize-buffer)))
    (with-current-buffer htmlized-buffer
      (let ((result-name-hash (sha1 (current-buffer))))
        (write-file (format fwoar-pastebin-tramp-url
                            result-name-hash
                            extension))
        (message "Wrote file to: %s.%s.html" result-name-hash extension)
        (browse-url (format fwoar-pastebin-web-url-pattern
                            result-name-hash
                            extension))))))

(provide 'fwoar-pastebin)
;;; fwoar-pastebin.el ends here
