;;;; -*- mode: Emacs-Lisp;-*-

(setq default-directory "~/emacs-home/")
(make-directory default-directory t)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

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
(use-package evil
  :ensure t
  :config
  (define-key evil-normal-state-map " o" 'slime-selector)
  (define-key evil-insert-state-map (kbd "TAB") 'company-complete)

  (use-package lisp-skeletons
    :ensure t
    :config
    (add-hook 'skeleton-end-hook 'skeleton-make-markers))
  (defun helm-generate-lisp-skeleton ()
    (interactive)
    (let ((skeletons '(("defunction" . skel-defun)
                       ("defmacro" . skel-defmacro)
                       ("defparameter" . skel-defparameter)
                       ("defvar" . skel-defvar))))
      (funcall (helm-comp-read "code template: " skeletons))))
  
  (define-key evil-normal-state-map " g" 'helm-generate-lisp-skeleton)
  (define-key evil-visual-state-map " g" 'helm-generate-lisp-skeleton)
  )

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

;;;;; SLIME SETUP {{{
(progn ;slime isn't loaded via use-package because quicklisp-helper keeps it uptodate
  (load (expand-file-name "~/quicklisp/slime-helper.el"))

  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "~/sbcl/bin/sbcl")

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
	  slime-mdot-fu)))

(use-package slime-company
  :no-require t
  :defer t
  :ensure t)

;;;;; }}}
(use-package company
  :ensure t
  :config
  ;; keybindings
  (progn (define-key company-active-map (kbd "C-c h") 'company-quickhelp-manual-begin)
	 (define-key company-active-map (kbd "SPC") (kbd "RET SPC"))
	 (define-key company-active-map (kbd "(") (kbd "RET SPC ("))
	 (define-key company-active-map (kbd "{") (kbd "RET SPC {"))
	 (define-key company-active-map (kbd "[") (kbd "RET [")))

  (message "backends: %s" company-backends)
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

;; NOTE: this must be here...
(slime-setup)

(global-company-mode 1)

(use-package projectile
  :ensure t)

(use-package sbt-mode
  :ensure t
  :pin melpa-stable)

(use-package scala-mode
  :ensure t
  :pin melpa-stable)

(use-package ensime
  :ensure t)

(use-package intero
  :ensure t)

;; (use-package jdee
;;   :ensure t)

(use-package mvn
  :ensure t)

(use-package css-eldoc
  :ensure t)

(use-package ag
  :ensure t)

(use-package evil-vimish-fold
  :ensure t
  :config
  (evil-vimish-fold-mode 1))

(use-package zenburn-theme
  :ensure t)

(use-package zeal-at-point
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package tabbar
  :ensure t)

(use-package helm
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package evil-visual-mark-mode
  :ensure t)

(advice-add 'evil-delete-marks :after
	    (lambda (&rest args)
	      (evil-visual-mark-render)))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys t))

(use-package paredit
  :ensure t
  :config
  (defun paredit-wiggle-back ()
    (paredit-forward)
    (paredit-backward))

  (defmacro defparedit-wrapper (name invoked-wrapper)
    `(defun ,name ()
       (interactive)
       (paredit-wiggle-back)
       (,invoked-wrapper)))

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

(use-package  evil-paredit
  :ensure t)

(use-package evil-numbers
  :ensure t
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package eldoc-eval
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package csv-mode
  :ensure t)

(use-package color-theme
  :ensure t)

(use-package ansible
  :ensure t)

(use-package alect-themes
  :ensure t)

(use-package ac-js2
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package vue-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package scss-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package  markdown-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package highlight-parentheses
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package helm-ls-git
  :ensure t)

;;(use-package helm-git
;;  :ensure t)

(use-package helm-css-scss
  :ensure t)

(use-package helm-cider
  :ensure t)

(use-package helm-ag-r
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package evil-surround
  :ensure t)

(use-package evil-leader
  :ensure t)

(use-package emmet-mode
  :ensure t)

(use-package project-explorer
  :ensure t
  )

(use-package ggtags
  :ensure t
  :config
  (ggtags-mode 1)
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
		(ggtags-mode 1)))))



(use-package pollen-mode
  :config
  (defun insert-lozenge ()
    (interactive)
    (insert-char 9674))
  (define-key evil-insert-state-map (kbd "C-c C-l") 'insert-lozenge))

(progn ; helm
  (require 'helm-config)
  (helm-mode)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key evil-normal-state-map " f" 'helm-projectile)
  (define-key evil-normal-state-map " j" 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x))

(require 'js2-mode)

(require 'projectile)
(projectile-mode)



(add-hook 'after-init-hook
	  (lambda ()
	    (unless (server-running-p)
	      (server-start))
	    (evil-mode)
	    (paredit-mode)
	    (evil-paredit-mode)
	    (global-company-mode)
	    (global-linum-mode)))

(progn ; linum
  (setq linum-format "%5d\u2502"))

(require 'evil-numbers)

(define-key evil-normal-state-map "ZZ" 'save-buffer)

(define-key evil-normal-state-map ",zz" 'zeal-at-point)

(modify-syntax-entry ?_ "w" js-mode-syntax-table)
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
(modify-syntax-entry ?@ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?[ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?] "w" lisp-mode-syntax-table)
(modify-syntax-entry ?^ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?~ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?{ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?} "w" lisp-mode-syntax-table)
(modify-syntax-entry ?. "w" lisp-mode-syntax-table)

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (make-directory default-directory t)
  (normal-top-level-add-subdirs-to-load-path))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight semi-bold :height 113 :width normal)))))
 

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; (defun znc-erc ()
;;   (interactive)
;;   (erc-ssl :server "localhost" :port 6697 :nick "edwlan/freenode" :password "t31ch3rtb"))

(add-to-list 'auto-mode-alist '("\\.cljs\\.hl\\'" . clojurescript-mode))

(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (eldoc-mode)
	     (robe-mode)))


(add-hook 'clojure-mode-hook
	  '(lambda ()
	     ;; Hoplon functions and macros
	     (paredit-mode)
	     (define-key evil-insert-state-map "^N" 'helm-cider-apropos)
	     (dolist (pair '((page . 'defun)
			     (loop-tpl . 'defun)
			     (if-tpl . '1)
			     (for-tpl . '1)
			     (case-tpl . '1)
			     (cond-tpl . 'defun)))
	       (put-clojure-indent (car pair)
				   (car (last pair))))))

(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

(global-auto-revert-mode t)

 ;;; Use auto-complete for ensime

(defun scala/enable-eldoc ()
  "Show error message or type name at point by Eldoc."
  (setq-local eldoc-documentation-function
	      #'(lambda ()
		  (when (ensime-connected-p)
		    (let ((err (ensime-print-errors-at-point)))
		      (or (and err (not (string= err "")) err)
			  (ensime-print-type-at-point))))))
  (eldoc-mode +1))

(defun scala/completing-dot-company ()
  (cond (company-backend
	 (company-complete-selection)
	 (scala/completing-dot))
	(t
	 (insert ".")
	 (company-complete))))

(defun scala/completing-dot-ac ()
  (insert ".")
  (ac-trigger-key-command t))

;; Interactive commands

(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (eval-and-compile (require 'ensime))
  (eval-and-compile (require 's))
  (when (s-matches? (rx (+ (not space)))
		    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (cond ((not (and (ensime-connected-p) ensime-completion-style))
	 (insert "."))
	((eq ensime-completion-style 'company)
	 (scala/completing-dot-company))
	((eq ensime-completion-style 'auto-complete)
	 (scala/completing-dot-ac))))

;; Initialization
(add-hook 'ensime-mode-hook #'scala/enable-eldoc)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'flycheck-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (semantic-mode 1)
	      
	      (global-semanticdb-minor-mode 1)
	      (global-semantic-idle-scheduler-mode 1)
	      (global-semantic-stickyfunc-mode 1)
	      
	      (helm-gtags-mode)
	      (ggtags-mode 1))))

(setq company-backends (delete 'company-semantic company-backends))

(defun alexott/cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'c-mode-hook 'alexott/cedet-hook)
(add-hook 'c++-mode-hook 'alexott/cedet-hook)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
 
 This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

(require 'ede)
(global-ede-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "x-www-browser")
 '(company-backends
   (quote
    (company-semantic company-bbdb company-nxml company-css company-xcode company-cmake company-capf company-files
                      (company-dabbrev-code company-gtags company-etags company-keywords)
                      company-oddmuse company-dabbrev)))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" default)))
 '(erc-modules
   (quote
    (autoaway autojoin button capab-identify completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring stamp spelling track)))
 '(erc-truncate-mode t)
 '(evil-leader/leader ",")
 '(evil-visual-mark-mode t)
 '(font-use-system-font t)
 '(ggtags-sort-by-nearness t)
 '(ggtags-update-on-save t)
 '(global-evil-surround-mode t)
 '(global-linum-mode t)
 '(haskell-mode-hook
   (quote
    (capitalized-words-mode haskell-decl-scan-mode haskell-indentation-mode highlight-uses-mode imenu-add-menubar-index interactive-haskell-mode)) t)
 '(helm-ls-git-fuzzy-match t)
 '(jdee-server-dir "~/.emacs.d/jdee-server/")
 '(jira-url "https://atomampd.atlassian.net/rpc/xmlrpc")
 '(line-number-mode nil)
 '(mac-command-modifier nil)
 '(mac-option-modifier (quote meta))
 '(mac-pass-command-to-system t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (deft geiser pollen-mode js2-refactor npm-mode helm-dash aggressive-indent company-tern tern srefactor ac-slime znc helm-ag ag helm-projectile notmuch zenburn-theme zeal-at-point use-package tabbar slime-company rainbow-delimiters projectile mvn jdee intero helm evil-visual-mark-mode evil-vimish-fold evil-paredit evil-numbers ensime eldoc-eval editorconfig color-theme ansible alect-themes ac-js2)))
 '(pe/omit-gitignore t)
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I.")
     (Base . 10)
     (Package . CL-USER)
     (Syntax . COMMON-LISP))))
 '(slime-company-completion (quote fuzzy)))
