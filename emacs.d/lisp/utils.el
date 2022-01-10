;;; utils -- Summary
;;; Commentary:

(require 'cl)

;;; Code:

(defun fwoar/doc-for-resource-type (resource-type)
  (cl-format nil
             "https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/~{~a~^-~}"
             (list* "aws" "resource"
                    (cdr (s-split "::" (s-downcase resource-type))))))
(defun fwoar/document-cf (min max)
  "Jump to AWS Cloudformation docs for a resource type.

MIN: the point in the buffer where the resource type starts
MAX: the point in the buffer where the resource type ends

This uses (interactive \"r\") to automagically populate the arguments
from the selected region."
  (interactive "r")
  (browse-url
   (fwoar/doc-for-resource-type
    (buffer-substring min max))))

(defun read-sexps-in-buffer (buffer)
  (with-temp-buffer
    (save-excursion
      (insert "(")
      (insert-buffer buffer)
      (goto-char (point-max))
      (insert "\n)"))
    (read (current-buffer))))

(defun read-sexps-in-file (fn)
  (with-temp-buffer
    (save-excursion
      (insert "(")
      (insert-file fn)
      (goto-char (point-max))
      (insert "\n)"))
    (read (current-buffer))))

(defun read-strings-in-file (fn)
  (with-temp-buffer
    (insert-file fn)
    (mark-whole-buffer)
    (flush-lines "^[[:space:]]*$")
    (goto-char (point-min))
    (remove "" (s-lines (buffer-string)))))

(defun op--collect-args (body)
  (cl-flet ((walker (body &optional args)
                    (if (null body)
                        args
                      (if (symbolp body)
                          (when (eql ?\_ (elt (symbol-name body) 0))
                            (cons body args))
                        (if (listp body)
                            (append (op--collect-args (car body))
                                    (op--collect-args (cdr body))
                                    ))))))
    (sort (walker body)
          (lambda (a b)
            (< (string-to-number (subseq (symbol-name a) 1))
               (string-to-number (subseq (symbol-name b) 1)))))))

(defmacro op (&rest body)
  `(lambda ,(op--collect-args body)
     ,@body))

(defun blank-line-p ()
  (= (current-indentation)
     (- (line-end-position) (line-beginning-position))))

(defun helm-generate-lisp-skeleton ()
  (interactive)
  (let ((skeletons '(("defunction" . skel-defun)
                     ("defmacro" . skel-defmacro)
                     ("defsystem" . skel-defsystem)
                     ("defpackage" . skel-defpackage)
                     ("defparameter" . skel-defparameter)
                     ("defvar" . skel-defvar))))
    (funcall (helm-comp-read "code template: " skeletons))
    (evil-insert 1)))

(defun create-system-files ()
  (interactive)
  (save-excursion
    (mark-defun)
    (mapcar (lambda (it) (save-buffer (find-file (format "%s.lisp" (cadr it)))))
            (getf (cddar (read-from-string
                          (buffer-substring (point)
                                            (mark))))
                  :components)))
  (pop-mark))

(defmacro comment (&rest _))

(comment
 (defun paredit-wiggle-back ()
   (paredit-forward)
   (paredit-backward))

 (defmacro defparedit-wrapper (name invoked-wrapper)
   `(defun ,name ()
      (interactive)
      (paredit-wiggle-back)
      (,invoked-wrapper))))

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
  that used by the user's shell.

  This is particularly useful under Mac OSX, where GUI apps are not
  started from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string
           (concat "zsh -c '"
                   "  source ~/.zsh.d/dependencies/utils.zsh;"
                   "  source ~/.zsh.d/dependencies/path-setup.zsh;"
                   "  echo $PATH"
                   "'")))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defmacro ensure-use-packages (&rest packages)
  (list* 'progn
         (mapcar (lambda (pck)
                   `(use-package ,(car pck)
                      :ensure t
                      ,@(cdr pck)))
                 packages)))

(defmacro fwoar/defvaralias! (var val-var)
  `(progn
     (setq ,var ,val-var)
     (defvaralias ',var ',val-var)))

(defun setup-indentation ()
  (setq-default indent-tabs-mode nil
                tab-width 2)
  (fwoar/defvaralias! c-basic-offset tab-width)
  (fwoar/defvaralias! sh-basic-offset tab-width)
  (fwoar/defvaralias! js2-basic-offset tab-width)
  (fwoar/defvaralias! sgml-basic-offset tab-width)
  (fwoar/defvaralias! cperl-indent-level tab-width)
  nil)

(defun start-server ()
  (unless (fboundp 'server-running-p)
    (require 'server))
  (let ((server-name (if fwoar.is-ordinary
                         server-name
                       "notes")))
    (unless (server-running-p)
      (server-start))))

(defun post-init ()
  ;;(centaur-tabs-mode 1)
  (evil-mode 1)

  (setup-indentation)
  (start-server)

  (if (version<= "26.0.50" emacs-version)
      (global-display-line-numbers-mode)
    (setq linum-format "%5d\u2502")
    (global-linum-mode))
  (set-exec-path-from-shell-PATH)
  ;; NOTE: this must be here...
  (global-company-mode 1))

(defvar fwoar.is-ordinary)

(defun cold-boot ()
  ""
  (run-with-idle-timer 5 t 'garbage-collect)
  (setq fwoar.is-ordinary (not (string= invocation-name "EmacsNotes")))
  (add-hook 'after-init-hook 'post-init)
  (electric-indent-mode -1)
  (comment
   (electric-pair-mode -1))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (when-let ((ls-executable (executable-find "gls")))
    (setq insert-directory-program ls-executable))

  (setq default-directory "~/emacs-home/")
  (make-directory default-directory t)

  (setq vc-follow-symlinks t)

  (require 'package)

  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
        package-archive-priorities '(("melpa-stable" . 1)
                                     ("gnu" . 0)
                                     ("melpa" . 2)
                                     ("nongnu" . 3)))

  ;; (package-initialize 'no-activate)
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))

  (setq browse-url-browser-function 'browse-url-default-browser
        history-delete-duplicates t)

  (require 'use-package))

(defun raise-iterm ()
  (interactive)
  (shell-command "open -a iTerm.app"))
(define-key global-map (kbd "C-M-s-l") 'raise-iterm)
