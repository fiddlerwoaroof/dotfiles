(require 'cl)

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
    (mapcar (lambda (it) (save-buffer (find-file (format "%s.lisp" (cadr it)))))
            (getf (cddar (read-from-string
                          (buffer-substring (point)
                                            (mark))))
                  :components)))

(defun slime-ecl ()
  (interactive)
  (let ((inferior-lisp-program "ecl"))
    (slime)))

(defun slime-cmucl ()
  (interactive)
  (let ((inferior-lisp-program "cmucl"))
    (slime)))

(defun slime-sbcl ()
  (interactive)
  (let ((inferior-lisp-program "sbcl"))
    (slime)))

(defun slime-ccl ()
  (interactive)
  (let ((inferior-lisp-program "ccl"))
    (slime)))

(defun find-use-clause (current-form)
  (when current-form
    (destructuring-bind (discriminator . packages) current-form
      (case discriminator
        (:use (remove-if (op (or (eql :cl _)))
                         (cdr current-form)))
        (defpackage (find-use-clause
                     (find-if (lambda (f)
                                (and (listp f)
                                     (eql (car f) :use)))
                              '(defpackage :tracking-sim (:use :cl :alexandria :serapeum) (:export)))))))))

(defun load-package-uses ()
  (interactive)
  (slime-eval-async `(ql:quickload ',(find-use-clause (list-at-point)))))

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
         (replace-regexp-in-string "[ \t\n]*$" ""
                                   (shell-command-to-string
                                    "zsh -c 'source ~/.zsh.d/dependencies/utils.zsh;source ~/.zsh.d/dependencies/path-setup.zsh;echo $PATH'")
)))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun load-package-configuration (package)
  (load (concat "~/.emacs.d/lisp/configurations/"
                (symbol-name package)
                ".el")))

(defmacro ensure-use-packages (&rest packages)
  (list* 'progn
         (mapcar (lambda (pck)
                   `(use-package ,(car pck)
                      :ensure t
                      ,@(cdr pck)))
                 packages)))

(defun post-init ()
            ;;;;; INDENTATION SETUP  {{{
  (progn
    (setq-default indent-tabs-mode nil
                  tab-width 2)
    (defvaralias 'c-basic-offset 'tab-width)
    (defvaralias 'sh-basic-offset 'tab-width)
    (defvaralias 'js2-basic-offset 'tab-width)
    (defvaralias 'sgml-basic-offset 'tab-width)
    (defvaralias 'cperl-indent-level 'tab-width))
            ;;;;; }}}

  ;; (require 'projectile)
  ;; (require 'evil-numbers)
  (unless (fboundp 'server-running-p)
    (require 'server))
  (let ((server-name (if fwoar.is-ordinary
                         server-name
                       "notes")))
    (unless (server-running-p)
      (server-start)))
  (projectile-mode)
  (evil-mode)
  ;; (paredit-mode)
  ;;(global-company-mode)
  ;; (setq linum-format "%5d\u2502")
  (global-linum-mode)
  (set-exec-path-from-shell-PATH)
  ;; NOTE: this must be here...
  (global-company-mode 1)
  (slime-setup))

(defun cold-boot ()
  (setq fwoar.is-ordinary (not (string= invocation-name "EmacsNotes")))
  (add-hook 'after-init-hook 'post-init)

  (when (file-exists-p "/usr/local/bin/gls")
    (setq insert-directory-program "/usr/local/bin/gls"))

  (setq default-directory "~/emacs-home/")
  (make-directory default-directory t)

  (setq vc-follow-symlinks t)

  (require 'package)

  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("org" . "http://orgmode.org/elpa/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/"))
        package-archive-priorities '(("melpa-stable" . 1)
                                     ("gnu" . 0)
                                     ("melpa" . 2)
                                     ("org" . 3)))

  (package-initialize)
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package))

  (setq browse-url-browser-function
        'eww-browse-url)

  (require 'use-package))

