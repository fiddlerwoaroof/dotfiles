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
  (funcall (-compose (fwoar:exclude
                      (fwoar:matches-regex "/\\(.*[#]\\)"))
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
                    (when-let* ((fwoar::project fwoar-helm-project::*current-project*)
                                (fwoar::root (project-root fwoar-helm-project::*current-project*)))
                      (mapcar (lambda (it)
                                (cons (f-relative it fwoar::root)
                                      it))
                              (project-files fwoar::project)))))
    (action . helm-find-files-actions)))

(defvar fwoar::*helm-project-buffers-source*
  `((name . "Project Buffers")
    (candidates . (lambda ()
                    (when-let* ((fwoar::project fwoar-helm-project::*current-project*))
                      (funcall (-compose (fwoar:over (lambda (it)
                                                       (cons (buffer-name it)
                                                             it)))
                                         (lambda (it)
                                           (cl-sort it 'string-lessp
                                                    :key 'buffer-name))
                                         (fwoar:exclude
                                          (fwoar:on (fwoar:matches-regex "^ ")
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

(defun fwoar::project-find-file (fn)
  (when-let* ((fwoar::project (project-current))
              (default-directory (project-root fwoar::project)))
    (find-file fn)))

(defvar fwoar-helm-project::*current-project*)
(defun fwoar::helm-find-file-in-project ()
  (interactive)
  (let ((fwoar-helm-project::*current-project* (project-current nil)))
    (helm (list 'fwoar::*helm-project-files-source*
                'fwoar::*helm-project-buffers-source*
                'fwoar::*helm-project-known-projects*
                (helm-build-dummy-source
                    "Create Project File"
                  :action (helm-make-actions
                           "Make file" #'fwoar::project-find-file))))))

(defun fwoar::spotlight-validate-or-make-dummy-process (input)
  (cond
   ((< (length input) helm-rg-input-min-search-chars)
    (helm-rg--make-dummy-process
     input
     (format "must be at least %d characters" helm-rg-input-min-search-chars)))
   (t t)))

(defun fwoar::spotlight-search (s)
  (s-split "\n" (shell-command-to-string
                 (format "mdfind %s"
                         (shell-quote-argument s)))))

(defun fwoar::spotlight-search ()
  "Invoke ripgrep in `helm-rg--current-dir' with `helm-pattern'.
Make a dummy process if the input is empty with a clear message to the user."
  (let* ((input helm-pattern))
    (pcase-exhaustive (fwoar::spotlight-validate-or-make-dummy-process input)
      ((and (pred processp) x)
       (setq helm-rg--last-argv nil)
       x)
      (`t
       (let* ((rg-regexp (helm-rg--helm-pattern-to-ripgrep-regexp input))
              (argv (helm-rg--construct-argv rg-regexp))
              (real-proc (helm-rg--make-process-from-argv argv)))
         (setq helm-rg--last-argv argv)
         real-proc)))))

(defvar *fwoar::spotlight-source*
  (helm-build-async-source
      "find files with spotlight"
    :candidates-process 'fwoar::spotlight-search
    :action (helm-make-actions
             "Open" #'find-file)
    ))
(defun fwoar:spotlight-search ()
  (interactive)
  (helm (list )
        :input ":write-image"
        :prompt "Spotlight expression: "
        ))

(defun fwoar::initialize-fwoar-helm-project ()
  (message "initializing fwoar-helm-project %s" project-switch-commands)
  (define-key project-prefix-map
              "f"
              'fwoar::helm-find-file-in-project)

  (let ((it (assoc 'project-find-file
                   project-switch-commands)))
    (when it
      (rplaca it
              'fwoar::helm-find-file-in-project)))
  (message "done initializing fwoar-helm-project %s" project-switch-commands))

(provide 'fwoar-helm-project)
;;; fwoar-helm-project.el ends here
