;;; fwoar-helm-project.el --- helm project browser -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Version: 0.0.1
;; Keywords: helm,project
;; URL: https://fwoar.co
;; Package-Requires: (helm f dash project fwoar-functional-utils)

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
(require 'helm)
(require 'f)
(require 'dash)
(require 'project)
(require 'fwoar-functional-utils)

(defun fwoar::get-candidates ()
  (funcall (-compose (fwoar/exclude
                      (fwoar/matches-regex "/\\(.*[#]\\)"))
                     'project-files
                     'project-current)))

(defun fwoar::browse-project ()
  (interactive)
  (if (package-installed-p 'projectile)
      (helm-projectile)
    (fwoar::helm-find-file-in-project)))

(defvar fwoar::*helm-project-files-source*
  `((name . "Project Files")
    (candidates . (lambda ()
                    (when-let* ((fwoar::project (project-current))
                                (fwoar::root (project-root fwoar::project)))
                      (mapcar (lambda (it)
                                (cons (f-relative it fwoar::root)
                                      it))
                              (project-files fwoar::project)))))
    (action . helm-find-files-actions)))

(defvar fwoar::*helm-project-buffers-source*
  `((name . "Project Buffers")
    (candidates . (lambda ()
                    (when-let* ((fwoar::project (project-current)))
                      (funcall (-compose (fwoar/over (lambda (it)
                                                       (cons (buffer-name it)
                                                             it)))
                                         (lambda (it)
                                           (cl-sort it 'string-lessp
                                                    :key 'buffer-name))
                                         (fwoar/exclude
                                          (fwoar/on (fwoar/matches-regex "^ ")
                                                    'buffer-name))
                                         'project-buffers)
                               fwoar::project))))
    (action . switch-to-buffer)))

(defvar fwoar::*helm-project-known-projects*
  `((name . "Projects")
    (candidates . project-known-project-roots)
    (action . (lambda (it)
                (comment (let ((default-directory it))
                           (fwoar::helm-find-file-in-project)))
                (project-switch-project it)))))

(defun fwoar::helm-find-file-in-project ()
  (interactive)
  (helm '(fwoar::*helm-project-files-source*
          fwoar::*helm-project-buffers-source*
          fwoar::*helm-project-known-projects*)))

(defun fwoar::initialize-fwoar-helm-project ()
  (message "initializing fwoar-helm-project %s" project-switch-commands)
  (define-key project-prefix-map
    "f" 'fwoar::helm-find-file-in-project)

  (let ((it (assoc 'project-find-file project-switch-commands)))
    (when it
      (rplaca it
              'fwoar::helm-find-file-in-project)))
  (message "done initializing fwoar-helm-project %s" project-switch-commands))

(provide 'fwoar-helm-project)
;;; fwoar-helm-project.el ends here
