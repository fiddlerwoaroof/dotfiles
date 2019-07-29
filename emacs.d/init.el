;; -*- mode: Emacs-Lisp;tab-width: 8;indent-tabs-mode: nil; -*-
(setq gc-cons-threshold 100000000)

(setq gc-cons-threshold 100000000
      inhibit-splash-screen t
      inhibit-startup-message t)

(let ((my-theme-path (expand-file-name "~/.emacs.d/themes/")))
  (add-to-list 'load-path my-theme-path)
  (add-to-list 'custom-theme-load-path my-theme-path)
  (load-theme 'fwoar-zenburn t))

(progn (setq default-frame-alist
             '((vertical-scroll-bars . nil)
               (right-divider-width . 2)
               (bottom-divider-width . 2)))
       (modify-all-frames-parameters default-frame-alist))

(tool-bar-mode 0)
(scroll-bar-mode 0)

(message invocation-name)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (make-directory default-directory t)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  (load "utils"))

(cold-boot)

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

(use-package org-projectile
  :config
  (progn
    (org-projectile-per-project)
    (setq org-agenda-skip-unavailable-files t)
    (setq org-projectile-per-project-filepath
          "notes/README.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)
    (define-key projectile-mode-map (kbd "C-c c") 'org-capture))
  :ensure t)

(use-package org-projectile-helm
  :after org-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c n p") 'org-projectile-helm-template-or-project))

(use-package deadgrep
  :ensure t)

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
  (add-hook 'company-mode-hook (lambda () (company-posframe-mode 1))))

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
  :config
  (modify-syntax-entry ?_ "w" js2-mode-syntax-table)
  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
  (setq-default js2-basic-offset 4)
  (setq-default js-indent-level 4)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (customize-set-variable 'js2-mode-show-parse-errors nil)
  (customize-set-variable 'js2-strict-missing-semi-warning nil)
  (customize-set-variable 'js2-strict-trailing-comma-warning nil)
  (customize-set-variable 'js2-strict-inconsistent-return-warning nil))

(use-package js
  :ensure t
  :config
  (modify-syntax-entry ?_ "w" js-mode-syntax-table)

  ;;; indent ternaries with arrow function correctly---
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
                   (memq (char-before) '(?, ?} ?{)))))))))

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
  (add-hook 'js2-mode-hook 'tide-hl-identifier-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode)))

(comment
 (use-package tern
   :config
   (add-hook 'js-mode-hook (lambda () (tern-mode t)))
   (add-hook 'js2-mode-hook (lambda () (tern-mode t)))))

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq company-tooltip-align-annotations t))

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
  :ensure t
  :config
  (require 'cider-selector)
  (define-key evil-normal-state-map " t" 'cider-test-run-ns-tests)
  (evil-define-key 'normal clojure-mode-map " '" 'helm-cider-apropos)

  (def-cider-selector-method ?S "find clojure project file"
    (fwoar--find-system))

  (add-hook 'cider-mode-hook
            (lambda ()
              (flycheck-mode)
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
              (aggressive-indent-mode 0)
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

  (defun cider-eval-expression-at-point-in-repl ()
    (interactive)
    (let ((form (cider-defun-at-point)))
      ;; Strip excess whitespace
      (while (string-match "\\`\s+\\|\n+\\'" form)
        (setq form (replace-match "" t t form)))
      (with-current-buffer (cider-current-repl nil t)
        (let ((fw/window (get-buffer-window)))
          (with-selected-window fw/window
            (end-of-buffer)
            (insert form)
            (cider-repl-return)
            (end-of-buffer))))))

  (define-key cider-mode-map
    (kbd "C-c C-j") 'cider-eval-expression-at-point-in-repl)

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

(use-package flycheck-clj-kondo
  :ensure t)




(use-package imenu
  :config
  (define-key evil-normal-state-map " d" 'helm-imenu-in-all-buffers))

(defun fwoar--paste-from-register-mru-buffer (register)
  (interactive
   (let ((overlay (make-overlay (point) (point)))
         (string "\""))
     (unwind-protect
         (progn
           ;; display " in the buffer while reading register
           (put-text-property 0 1 'face 'minibuffer-prompt string)
           (put-text-property 0 1 'cursor t string)
           (overlay-put overlay 'after-string string)
           (list (or evil-this-register (read-char))))
       (delete-overlay overlay))))
  (let ((filename (with-current-buffer helm-current-buffer 
                    (evil-get-register register t))))
    (insert filename)))

(use-package ag :ensure t)

(use-package aggressive-indent :ensure t)

(use-package cl-format :ensure t)

(use-package css-eldoc :ensure t)

(use-package csv-mode :ensure t)

(use-package eldoc-eval :ensure t)

(use-package helm :ensure t :config
  (require 'helm-config)
  (helm-mode)
  (global-set-key
   (kbd "M-x")
   'helm-M-x)
  (global-set-key
   (kbd "C-x C-f")
   'helm-find-files)
  (define-key evil-normal-state-map " f" 'helm-projectile)
  (define-key evil-normal-state-map " j" 'helm-buffers-list)
  (define-key evil-normal-state-map " s" 'helm-occur)
  (define-key evil-normal-state-map " S" 'helm-projectile-rg)
  (define-key helm-map
    (kbd "C-r")
    'fwoar--paste-from-register-mru-buffer)
  (define-key helm-map
    (kbd "<right>")
    'helm-execute-persistent-action)
  (define-key helm-map
    (kbd "<left>")
    'helm-find-files-up-one-level))

(use-package helm-ag :ensure t)

(use-package helm-ag-r :ensure t)

(use-package helm-rg :ensure t)

(use-package helm-cider :ensure t)

(use-package helm-css-scss :ensure t)

(use-package helm-ls-git :ensure t)

(use-package helm-projectile :ensure t)

(use-package helm-projectile :ensure t)

(use-package highlight-parentheses :ensure t :config
  (global-highlight-parentheses-mode 1))

(use-package magit :ensure t :config
  (evil-define-key 'normal magit-file-mode-map " a" 'magit)
  (magit-define-popup-action 'magit-dispatch-popup 106 "Browse remote" 'browse-at-remote)
  'magit-dispatch)

(use-package markdown-mode :ensure t)

(use-package project-explorer :ensure t)

(use-package rainbow-delimiters :ensure t)

(use-package ripgrep :ensure t)

(use-package projectile-ripgrep :ensure t)

(use-package scss-mode :ensure t)

(use-package smartparens :ensure t :ensure t :config
  (sp-pair "${" "}")
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

(use-package web-mode :ensure t)

(use-package yaml-mode :ensure t)



(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(defun fwoar--activate-treemacs ()
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (delete-window)
    (treemacs-select-window)))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-is-never-other-window t)
  (global-set-key (kbd "s-e") 'fwoar--activate-treemacs))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(progn ;; emacs-lisp stuff
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)

  (put 'narrow-to-page 'disabled nil)
  )

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

(comment
 (use-package circe
   :config
   (setq circe-server-buffer-name "{host}:{port}"
         circe-reduce-lurker-spam t
         circe-network-options (read-sexps-in-file "~/.circe-info"))))

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
  (define-key global-map "\C-c[" "\C-u-1\C-xnp\M-<"))


;;;;; junk drawer ....


(defun fwoar/zenburn-css ()
  (interactive)
  (mapcar (lambda (desc)
            (destructuring-bind (name . value) desc
              (cl-format (current-buffer)
                         "--~a: ~a;~%"
                         (s-replace "+" "-plus-" name)
                         value)))
          fwoar-zenburn-default-colors-alist))

(defun fwoar/camel-kebab (string)
  (let ((case-fold-search nil))
    (downcase
     (format "%c%s"
             (elt string 0)
             (or (when (> (length string) 1)
                   (s-replace-regexp "[A-Z]"
                                     "-\\&"
                                     string nil nil nil 1))
                 string)))))

(defun fwoar/cc-camel-kebab (start end)
  (interactive "*r")
  (let ((target (buffer-substring start end)))
    (save-excursion
      (delete-region start end)
      (insert (fwoar/camel-kebab target)))))

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

(use-package org-brain :ensure t
  :init
  (setq org-brain-path "~/org-brain/")

  :config
  (eval-after-load 'evil
    '(evil-set-initial-state 'org-brain-visualize-mode 'emacs))

  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)

  (setq org-id-track-globally t
        org-id-locations-file "~/.emacs.d/.org-id-locations"
        org-brain-visualize-default-choices 'all
        org-brain-title-max-length 12))

(setq diary-file (expand-file-name "~/bucket/diary"))
