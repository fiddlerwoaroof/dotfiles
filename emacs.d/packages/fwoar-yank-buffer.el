;;; fwoar-yank-buffer.el --- commands to -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Version: 0.0.1
;; Keywords: yank,buffer
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

;; Random utilities for yanking buffer names

;;; Code:

;;;###autoload
(defun fwoar/yank-buffer-file ()
  (interactive)
  (kill-new buffer-file-name))

;;;###autoload
(defun fwoar/yank-buffer-directory ()
  (interactive)
  (kill-new default-directory))

;;;###autoload
(defun fwoar/yank-relative-to-project ()
  (interactive)
  (let ((default-directory (project-root (project-current))))
    (kill-new (file-relative-name buffer-file-name))))

;;;###autoload
(defun fwoar/yank-buffer-name ()
  (interactive)
  (kill-new (buffer-name)))

(provide 'fwoar-yank-buffer)
;;; fwoar-yank-buffer.el ends here
