;; -*- mode: Emacs-Lisp;tab-width: 8;indent-tabs-mode: nil; -*-
(setq gc-cons-threshold 100000000)
(message invocation-name)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;(let ((file-name-handler-alist nil))

(setq fwoar.is-ordinary (not (string= invocation-name "EmacsNotes")))
(add-hook 'after-init-hook
          (lambda ()
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
            (paredit-mode)
            ;;(global-company-mode)
            (setq linum-format "%5d\u2502")
            (global-linum-mode)
            (set-exec-path-from-shell-PATH)

            ;; NOTE: this must be here...
            (slime-setup)

            (global-company-mode 1)

            ))

(when (file-exists-p "/usr/local/bin/gls")
  (setq insert-directory-program "/usr/local/bin/gls"))


(let ((default-directory  "~/.emacs.d/lisp/"))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)

  (load "utils"))

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



(require 'use-package)
(use-package color-theme :ensure t)
(use-package zenburn-theme :ensure t)
(color-theme-initialize)
(load-theme 'zenburn t)

;(use-package erc
;  :config
;  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer))
(use-package evil
  :ensure t
  :config

  (advice-add 'evil-delete-marks :after
              (lambda (&rest args)
                (evil-visual-mark-render)))

  (define-key evil-normal-state-map " o" 'slime-selector)
  (define-key evil-insert-state-map (kbd "TAB") 'company-indent-or-complete-common)
  (evil-mode)
  (use-package  evil-paredit
    :ensure t
    :after paredit
    :config
    (evil-paredit-mode))


  ;;(use-package evil-numbers
  ;;  :ensure t
  ;;  :config
  ;;  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  ;;  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")))

(use-package emmet-mode
  :ensure t
  :config
  (define-key evil-insert-state-map (kbd "C-c ,") 'emmet-expand-line)
  )

(use-package lisp-skeletons
  :config
  (add-hook 'skeleton-end-hook 'skeleton-make-markers)

  (define-key evil-insert-state-map (kbd "C-c j") 'skeleton-next-position)
  (define-key evil-insert-state-map (kbd "C-c k") 'skeleton-prev-position)
  (define-key evil-normal-state-map " g" 'helm-generate-lisp-skeleton)
  (define-key evil-visual-state-map " g" 'helm-generate-lisp-skeleton))

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

(setq browse-url-browser-function
      'eww-browse-url)

;;;; SLIME SETUP {{{
(use-package slime-company
  :no-require t
  :ensure t)

(load (expand-file-name "~/quicklisp/slime-helper.el"))

(run-with-idle-timer
 5 nil  ;slime isn't loaded via use-package because quicklisp-helper keeps it uptodate
 (lambda ()

   (when (and (boundp 'common-lisp-hyperspec-root)
              (string-prefix-p "/" common-lisp-hyperspec-root))
     (setq common-lisp-hyperspec-root
           (concat "file://" common-lisp-hyperspec-root)))

   ;; Replace "sbcl" with the path to your implementation
   (setq inferior-lisp-program "~/sbcl/bin/sbcl")

   (add-hook 'lisp-mode-hook
             '(lambda ()
                ;;(define-key evil-insert-state-map "^N" 'slime-fuzzy-indent-and-complete-symbol)
                (unless (string= "*slime-scratch*" (buffer-name))
                  (paredit-mode)
                  (evil-paredit-mode))
                (rainbow-delimiters-mode))) 
   (setq slime-contribs
         '(slime-fancy
           slime-company
           slime-macrostep
           slime-trace-dialog
           slime-mdot-fu))
   

   (modify-syntax-entry ?- "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?* "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?+ "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?! "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?$ "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?% "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?& "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?% "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?= "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?< "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?> "w" lisp-mode-syntax-table)
   ;;(modify-syntax-entry ?@ "w" lisp-mode-syntax-table)

   (modify-syntax-entry ?^ "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?~ "w" lisp-mode-syntax-table)
   (modify-syntax-entry ?. "w" lisp-mode-syntax-table)

   ))

 ;;;;; }}}
(use-package company
  :defer 5
  :config
  ;; keybindings
  (progn (define-key company-active-map (kbd "C-c h") 'company-quickhelp-manual-begin)
         (define-key company-active-map (kbd "SPC") (kbd "RET SPC"))
         (define-key company-active-map (kbd "(") (kbd "RET SPC ("))
         (define-key company-active-map (kbd "{") (kbd "RET SPC {"))
         (define-key company-active-map (kbd "[") (kbd "RET [")))

  ;;(message "backends: %s" company-backends)
  (setq company-backends
        '(company-clang
          company-bbdb
          company-nxml
          company-css
          company-xcode
          company-cmake
          company-capf
          company-files
          (company-dabbrev-code
           company-gtags
           company-etags
           company-keywords)
          company-oddmuse
          company-dabbrev))
  )

(use-package projectile
  :ensure t
  :config
  (projectile-register-project-type
   'lisp '("*.asd"))
  
  (projectile-register-project-type
   'npm '("package.json")
   :compile "npm install"
   :test "npm test"
   :run "npm start"
   :test-suffix ".spec"))

(defmacro ensure-use-packages (&rest packages)
  (list* 'progn
         (mapcar (op `(use-package ,_
                        :ensure t))
                 packages)))

(use-package css-eldoc :ensure t)
(use-package ag :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package helm :ensure t)
(use-package helm-projectile :ensure t)
(use-package eldoc-eval :ensure t)
(use-package csv-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package web-mode :ensure t)
(use-package vue-mode :ensure t)
(use-package scss-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package magit :ensure t :defer 2)
(use-package highlight-parentheses :ensure t)
(use-package helm-projectile :ensure t)
(use-package helm-ls-git :ensure t)
(use-package helm-css-scss :ensure t)
;;(use-package ac-js2 :ensure t)
;;(use-package helm-cider :ensure t :defer 5)
(use-package helm-ag-r :ensure t)
(use-package helm-ag :ensure t)
(use-package project-explorer :ensure t)


(progn ; helm
  (require 'helm-config)
  (helm-mode)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key evil-normal-state-map " f" 'helm-projectile)
  (define-key evil-normal-state-map " j" 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x))


(use-package paredit
  :ensure t
  :config
  (defparedit-wrapper back-then-wrap paredit-wrap-sexp)
  (defparedit-wrapper back-then-wrap-square paredit-wrap-square)
  (defparedit-wrapper back-then-wrap-curly paredit-wrap-curly)
  (defparedit-wrapper back-then-wrap-angled paredit-wrap-angled)
  (defparedit-wrapper back-then-wrap-doublequote paredit-meta-doublequote)
  (define-key evil-normal-state-map ",W" 'back-then-wrap)
  (define-key evil-normal-state-map ",w]" 'back-then-wrap-square)
  (define-key evil-normal-state-map ",w}" 'back-then-wrap-curly)
  (define-key evil-normal-state-map ",w>" 'back-then-wrap-angled)
  (define-key evil-normal-state-map ",w\"" 'back-then-wrap-doublequote)

  (define-key evil-normal-state-map ",S" 'paredit-splice-sexp)
  (define-key evil-normal-state-map ",A" 'paredit-splice-sexp-killing-backward)
  (define-key evil-normal-state-map ",D" 'paredit-splice-sexp-killing-forward)
  (define-key evil-normal-state-map ",|" 'paredit-split-sexp)
  (define-key evil-normal-state-map ",J" 'paredit-join-sexps)
  (define-key evil-normal-state-map ",<" 'paredit-backward-slurp-sexp)
  (define-key evil-normal-state-map ",," 'paredit-backward-barf-sexp) 
  (define-key evil-normal-state-map ",>" 'paredit-forward-slurp-sexp)
  (define-key evil-normal-state-map ",." 'paredit-forward-barf-sexp) 
  (define-key evil-normal-state-map ",~" 'paredit-convolute-sexp))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(define-key evil-normal-state-map "ZZ" 'save-buffer)

;;(modify-syntax-entry ?_ "w" js-mode-syntax-table)

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)

;;)

(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(defun fwoar--slime-find-system ()
  (let ((systems (directory-files
                  (locate-dominating-file default-directory
                                          (lambda (n)
                                            (directory-files n nil "^[^.#][^#]*[.]asd$")))
                  t "^[^.#][^#]*[.]asd$")))
    (find-file (if (not (null (cdr systems)))
                   (helm-comp-read "system:" systems)
                 (car systems)))))

(pushnew (list ?S "Goto System" #'fwoar--slime-find-system)
         slime-selector-methods
         :key #'car)

(defun ansi-term-post (&rest r)
  (message "Loading ansi term...")
  (evil-set-initial-state 'term-mod 'emacs))

(advice-add 'ansi-term :after 'ansi-term-post)

(defun edit-init-el ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(setq gc-cons-threshold (* 100 1024))

(unless fwoar.is-ordinary
  (setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")
  (require 'cjpad)
  (find-file "~/notes.org"))
