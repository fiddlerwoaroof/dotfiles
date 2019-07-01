;; -*- mode: Emacs-Lisp;tab-width: 8;indent-tabs-mode: nil; -*-

(setq gc-cons-threshold 100000000)

(tool-bar-mode 0)
(scroll-bar-mode 0)

(defvar zen-mode)

(message invocation-name)
(setq gc-cons-threshold 100000000)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  (load "utils"))

(cold-boot)

(use-package color-theme-modern :ensure t)
(use-package zenburn-theme :ensure t)
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(require 'el-zenburn-theme)
;;(color-theme-initialize)

(load-theme 'el-zenburn t)

(load-package-configuration 'evil)

(use-package multifiles
  :config
  (define-key evil-visual-state-map " m" 'mf/mirror-region-in-multifile)
  )

(use-package org
  :pin "org"
  :ensure t
  :config
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/scratch.org")
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-log-done 'time
        org-capture-templates '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
                                 "* TODO %?\n  %i\n  %a")
                                ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
                                 "* %?\nEntered on %U\n  %i\n  %a")
                                ("s" "Snippet" entry (file "~/org/snippets.org")
                                 "* %?\n#+BEGIN_SRC\n%i\n#+END_SRC")
                                ("b" "Bookmarks" entry (file+olp+datetree "~/org/bookmarks.org")
                                 "* %? %^g\n%c\n"))
        org-refile-targets '((nil . (:maxlevel . 2))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)))

  (define-key global-map "\C-cc" 'org-capture)
  (define-key evil-visual-state-map " c" 'org-capture))

(use-package deft
  :ensure t
  :config
  (define-key evil-normal-state-map " v" 'deft))

(use-package emmet-mode
  :ensure t
  :config
  (define-key evil-insert-state-map (kbd "C-c ,") 'emmet-expand-line))

(use-package company-posframe
  :ensure t)
(use-package company
  :config
  ;; keybindings
  (progn (define-key company-active-map (kbd "C-c h") 'company-quickhelp-manual-begin)
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
          company-slime
          company-files
          (company-dabbrev-code
           company-gtags
           company-etags
           company-keywords)
          company-oddmuse
          company-dabbrev))
  (add-hook 'company-mode-hook (lambda () (company-posframe-mode 1)))
  )

(use-package lisp-skeletons
  :config
  (add-hook 'skeleton-end-hook 'skeleton-make-markers)

  (define-key evil-insert-state-map (kbd "C-c j") 'skeleton-next-position)
  (define-key evil-insert-state-map (kbd "C-c k") 'skeleton-prev-position)
  (define-key evil-normal-state-map " g" 'helm-generate-lisp-skeleton)
  (define-key evil-visual-state-map " g" 'helm-generate-lisp-skeleton))

(load-package-configuration 'slime)
(global-company-mode 1)



(use-package js2-mode
  :ensure t
  :defer t
  :commands js2-mode
  :init
  (progn
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    (setq-default js2-basic-offset 4)
    (setq-default js-indent-level 4)
    (add-hook 'js2-mode-hook 'flycheck-mode)
    (customize-set-variable 'js2-mode-show-parse-errors nil)
    (customize-set-variable 'js2-strict-missing-semi-warning nil)
    (customize-set-variable 'js2-strict-trailing-comma-warning nil)
    (customize-set-variable 'js2-strict-inconsistent-return-warning nil)))

(use-package vue-mode
  :config
  (add-hook 'vue-mode
            'prettier-js-mode)
  (add-hook 'vue-mode
            'flycheck-mode))

(use-package prettier-js
  :ensure t
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package tide
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'tide-setup)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode)))

(comment
 (use-package tern
   :config
   (add-hook 'js-mode-hook (lambda () (tern-mode t)))
   (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

 (use-package company-tern
   :ensure t
   :config
   (add-to-list 'company-backends 'company-tern)
   (setq company-tooltip-align-annotations t)))



(defun more-than-one-project-file-p ()
  (= (length (projectile-select-files (projectile-current-project-files)))
     1))

(defun global-find-known-file ())

(defun helm-find-known-file (&optional arg)
  "Use projectile with Helm for finding files in project

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (let ((projectile-enable-caching t))
    (if (projectile-project-p)
        (projectile-maybe-invalidate-cache arg)
      (unless t
        (error "You're not in a project"))))
  (let ((helm-ff-transformer-show-only-basename nil)
        (helm-boring-file-regexp-list nil))
    (helm :sources 'helm-source-projectile-files-in-all-projects-list
          :buffer (concat "*helm projectile: "
                          (projectile-project-name)
                          "*")
          :truncate-lines helm-projectile-truncate-lines
          :prompt (projectile-prepend-project-name "Find file in projects: "))))

(defun project-aware-ffap (&rest args)
  (interactive "F")
  (apply (if (and (projectile-project-p)
                  (more-than-one-project-file-p))
             'helm-projectile-find-file-dwim
           'find-file-at-point)
         args))

(use-package projectile
  :ensure t
  :config
  (define-key evil-normal-state-map " h" 'helm-projectile-find-file-dwim)
  (setq
   ;;       projectile-enable-caching t
   projectile-generic-command "rg --files -0"
   )

  (projectile-register-project-type
   'clojure '("project.clj")
   :compile "lein uberjar"
   :test-dir "src/test/")

  (projectile-register-project-type
   'lisp '("*.asd"))
  
  (projectile-register-project-type
   'npm '("package.json")
   :compile "npm install"
   :test "npm test"
   :run "npm start"
   :test-suffix ".spec")

  (define-key evil-normal-state-map "gf" 'project-aware-ffap))



(use-package cl-generic
  :ensure t)

(defun intercept-print (f)
  (print f)
  f)

(cl-defmethod fwoar--find-system (&context (major-mode clojure-mode))
  (let ((systems (directory-files
                  (locate-dominating-file default-directory
                                          (lambda (n)
                                            (or (directory-files n nil "project.clj")
                                                (directory-files n nil "build.boot")
                                                (directory-files n nil "deps.edn")
                                                (directory-files n nil "shadow-cljs.edn"))))
                  t "^\\(project.clj\\|build.boot\\|deps.edn\\|shadow-cljs.edn\\)$")))
    (find-file (if (not (null (cdr systems)))
                   (helm-comp-read "system:" systems)
                 (car systems)))))

(def-cider-selector-method ?S "find clojure project file"
  (fwoar--find-system))

(cl-defgeneric fwoar--pl-selector ()
  (:method ()
           (slime-selector))
  (:method (&context (major-mode clojure-mode))
           (cider-selector))
  (:method (&context (projectile-project-type (eql :clojure)))
           (cider-selector))
  (:method (&context (major-mode cider-repl-mode))
           (cider-selector)))
(defun fwoar-pl-selector ()
  (interactive)
  (fwoar--pl-selector))
(define-key evil-normal-state-map " o" 'fwoar-pl-selector)



(use-package cider
  :config
  (define-key evil-normal-state-map " t" 'cider-test-run-ns-tests)
  (evil-define-key 'normal clojure-mode-map " '" 'helm-cider-apropos)

  (add-hook 'cider-mode-hook
            (lambda ()
              (rainbow-delimiters-mode 1)
              (evil-smartparens-mode 1)
              (smartparens-strict-mode 1)
              (aggressive-indent-mode 1)
              (helm-cider-mode 1)
              (cider-company-enable-fuzzy-completion)))

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (rainbow-delimiters-mode 1)
              (evil-smartparens-mode 1)
              (smartparens-strict-mode 1)
              (aggressive-indent-mode 1)
              (helm-cider-mode 1)
              (cider-company-enable-fuzzy-completion)))

  (modify-syntax-entry ?_ "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?~ "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?. "w" clojure-mode-syntax-table)

  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (define-key cider-repl-mode-map (kbd "C-c C-o") 'cider-repl-clear-output)
  (setq cider-save-file-on-load t
        cider-repl-history-file "~/.emacs.d/cider-history.clj")

  ;; https://github.com/clojure-emacs/cider/issues/2435
  (defun cider--gather-session-params (session)
    "Gather all params for a SESSION."
    (let (params)
      (dolist (repl (cdr session))
        (when (buffer-name repl)
          (setq params (cider--gather-connect-params params repl))))
      (when-let* ((server (cider--session-server session)))
        (setq params (cider--gather-connect-params params server)))
      params))
  )



(ensure-use-packages
 ;;(ac-js2)
 (ag)
 (aggressive-indent)
 (css-eldoc)
 (csv-mode)
 (eldoc-eval)
 (helm
  :config
  (require 'helm-config)

  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key evil-normal-state-map " f" 'helm-projectile)
  (define-key evil-normal-state-map " j" 'helm-buffers-list)
  (define-key evil-normal-state-map " s" 'helm-occur)
  (define-key evil-normal-state-map " S" 'helm-projectile-rg)
  (define-key helm-map (kbd "C-r") 'evil-paste-from-register))

 (helm-ag)
 (helm-ag-r)
 (helm-rg)
 (helm-cider)
 (helm-css-scss)
 (helm-ls-git)
 (helm-projectile)
 (helm-projectile)
 (highlight-parentheses
  :config
  (global-highlight-parentheses-mode 1))
 (magit
  :config
  (evil-define-key 'normal magit-file-mode-map " a" 'magit)
  (magit-define-popup-action 'magit-dispatch-popup ?j "Browse remote" 'browse-at-remote))
 (markdown-mode)
 (project-explorer)
 (rainbow-delimiters)
 (ripgrep)
 (projectile-ripgrep)
 (scss-mode)
 (smartparens
  :ensure t
  :config
  (sp-with-modes sp--lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  (add-hook 'smartparens-enabled-hook
            'evil-smartparens-mode))
 (web-mode)
 (yaml-mode))



(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))


(define-key evil-normal-state-map "ZZ" 'save-buffer)

(modify-syntax-entry ?_ "w" js-mode-syntax-table)
(modify-syntax-entry ?_ "w" js2-mode-syntax-table)

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)


;;)



(global-set-key (kbd "s-v") 'yank)

(setq custom-file "~/.emacs.d/custom.el")

(defvar *dotfiles-repo* "~/git_repos/dotfiles/")
(defun edit-init-el ()
  (interactive)
  (let ((default-directory *dotfiles-repo*))
    (helm-projectile-find-file)))

(setq gc-cons-threshold (* 100 1024))

(unless fwoar.is-ordinary
  (setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")
  (require 'cjpad)
  (find-file "~/org/notes.org"))

(put 'narrow-to-region
     'disabled
     nil)

(global-set-key (kbd "C-x C-b")
                'ibuffer)

(setq org-agenda-files '("~/org/notes.org"))

(load-file custom-file)

(defun read-sexps-in-file (fn)
  (with-temp-buffer
    (save-excursion
      (insert "(")
      (insert-file fn)
      (goto-char (point-max))
      (insert "\n)"))
    (read (current-buffer))))

(use-package circe
  :config
  (setq circe-server-buffer-name "{host}:{port}"
        circe-reduce-lurker-spam t
        circe-network-options (read-sexps-in-file "~/.circe-info")))

(defvar url-pattern (car (read-sexps-in-file "~/.pastebin-name")))
(defun pastebin-buffer ()
  (interactive)
  (let* ((extension (file-name-extension (elt (split-string (buffer-name) "<") 0)))
         (htmlized-buffer (htmlize-buffer)))
    (with-current-buffer htmlized-buffer
      (let ((result-name-hash (sha1 (current-buffer))))
        (write-file (format url-pattern result-name-hash extension))
        (message "Wrote file to: %s.%s.html" result-name-hash extension)
        (browse-url (format "https://fwoar.co/pastebin/%s.%s.html" result-name-hash extension))))))

(defun delete-mru-window ()
  (interactive)
  (delete-window
   (get-mru-window nil nil t)))
(define-key evil-motion-state-map (kbd "C-w C-o") 'delete-mru-window)
(define-key evil-motion-state-map (kbd "C-w C-w") 'evil-window-mru)

(defvar passwords ())
(defslimefun get-passwd (id prompt)
  (let ((val (assoc id passwords)))
    (cdr
     (if val val
       (car (push (cons id (read-passwd prompt))
                  passwords))))))

(add-to-list 'browse-url-filename-alist
             '("/Users/\\([^/]+\\)/\\(Sites\\|public_html\\)\\(/.*\\)" . "https://localhost/~\\1\\3"))

(progn ;; narrowing
  (define-key global-map "\C-c]" "\C-u1\C-xnp")
  (define-key global-map "\C-c[" "\C-u-1\C-xnp"))


;;;;; junk drawer ....

(defun js--looking-at-operator-p ()
  "Return non-nil if point is on a JavaScript operator, other than a comma."
  (save-match-data
    (and (looking-at js--indent-operator-re)
         (or (not (eq (char-after) ?:))
             (save-excursion
               (js--backward-syntactic-ws)
               (when (memq (char-before) '(?\) ?})) (backward-list))
               (and (js--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (eq (char-after) ??))))
         (not (and
               (eq (char-after) ?/)
               (save-excursion
                 (eq (nth 3 (syntax-ppss)) ?/))))
         (not (and
               (eq (char-after) ?*)
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" js--name-re " *(\\)"))
               (save-excursion
                 (js--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{))))))))


(progn ;; workaround until this fixed: https://github.com/emacs-evil/evil/issues/1129
  ;;                          or this: https://github.com/emacs-evil/evil/pull/1130
  (defun config/fix-evil-window-move (orig-fun &rest args)
    "Close Treemacs while moving windows around."
    (let* ((treemacs-window (treemacs-get-local-window))
           (is-active (and treemacs-window (window-live-p treemacs-window))))
      (when is-active (treemacs))
      (apply orig-fun args)
      (when is-active
        (save-selected-window
          (treemacs)))))

  (dolist (func '(evil-window-move-far-left evil-window-move-far-right evil-window-move-very-top evil-window-move-very-bottom))
    (advice-add func :around #'config/fix-evil-window-move)))

(defun fwoar--find-package-json ()
  (expand-file-name
   (locate-dominating-file default-directory
                           (lambda (n)
                             (directory-files n nil "^package.json$")))))



(defun find-package-json (default-directory)
  (interactive "D")
  (message "pakcage json: %s"(fwoar--find-package-json))
  (find-file (concat (fwoar--find-package-json)
                     "/package.json")))

(cl-defmethod fwoar--find-system (&context (major-mode (derived-mode js-mode)))
  (find-package-json default-directory))

(use-package jest
  :ensure t
  :config
  (defun jest--project-root ()
    "Find the project root directory."
    (let ((closest-package-json (fwoar--find-package-json))
          (projectile-root (projectile-project-root)))
      (message "%s <-> %s" closest-package-json projectile-root)
      (if (s-prefix-p projectile-root closest-package-json)
          closest-package-json
        projectile-root))))

(comment
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

 (use-package erc
   :config
   (add-hook 'erc-insert-post-hook 'erc-truncate-buffer))

 (use-package ansi-term :no-require t
   :config
   (eval-after-load 'evil
     (evil-set-initial-state 'term-mode 'emacs)))

 (defun ansi-term-post (&rest r)
   (message "Loading ansi term...")
   (evil-set-initial-state 'term-mode 'emacs))

 (advice-add 'ansi-term :after 'ansi-term-post)

 (use-package org-brain :ensure t
   :init
   (setq org-brain-path "~/org-brain/")

   :config
   ;; For Evil users
   (eval-after-load 'evil
     (evil-set-initial-state 'org-brain-visualize-mode 'emacs))

   (setq org-id-track-globally t)
   (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
   (push '("b" "Brain" plain (function org-brain-goto-end)
           "* %i%?" :empty-lines 1)
         org-capture-templates)
   (setq org-brain-visualize-default-choices 'all)
   (setq org-brain-title-max-length 12)))

(put 'narrow-to-page 'disabled nil)
