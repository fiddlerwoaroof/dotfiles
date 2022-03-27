;; -*- mode: Emacs-Lisp;tab-width: 8;indent-tabs-mode: nil; -*-
(require 'cl-generic)
(cl-defgeneric (setf seq-elt) (store sequence n))

(setq read-process-output-max (* 1024 1024))
(setq
 gc-cons-threshold 100000000
 load-prefer-newer t
 inhibit-splash-screen t
 inhibit-startup-message t)

(let ((my-theme-path (expand-file-name "~/.emacs.d/themes/")))
  (add-to-list 'load-path my-theme-path)
  (add-to-list 'custom-theme-load-path my-theme-path)
  (load-theme 'fwoar-zenburn t))

(progn
  (define-key key-translation-map (kbd "M-8") (kbd "•"))
  (define-key key-translation-map (kbd "M-9") (kbd "λ")))

(message invocation-name)

(let ((default-directory  "~/.emacs.d/lisp/"))
  (make-directory default-directory t)
  (add-to-list 'load-path (expand-file-name default-directory))
  (normal-top-level-add-subdirs-to-load-path)
  (load "utils"))

(set-exec-path-from-shell-PATH)
(cold-boot)

(use-package nix-mode
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package keyfreq
  :ensure t
  :diminish
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package auto-package-update
  :ensure t)
(use-package general
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package company
  :ensure t
  :delight
  :config
  ;; keybindings
  (progn (define-key company-active-map (kbd "C-c h") 'company-quickhelp-manual-begin)
         (define-key company-active-map (kbd "M-.") 'company-show-location)
         (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
         (define-key company-active-map (kbd "(") (kbd "RET SPC ("))
         (define-key company-active-map (kbd "{") (kbd "RET SPC {"))
         (define-key company-active-map (kbd "[") (kbd "RET [")))

  (setq company-backends '((company-clang
                            company-bbdb
                            company-nxml
                            company-css
                            ;;company-xcode
                            company-cmake
                            company-capf
                            ;;company-slime
                            )
                           company-files
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-oddmuse
                           company-dabbrev)))

(use-package company-posframe
  :ensure t
  :delight
  :after company
  :config
  (add-hook 'company-mode-hook (lambda () (company-posframe-mode 1)))
  (setq company-posframe-quickhelp-delay nil))



(use-package jeison
  :ensure t)

(use-package scala-mode
  :ensure t)



(eval-and-compile
  (defvar *fwoar-git-repos*
    (file-name-as-directory
     (expand-file-name (car (file-expand-wildcards "~/git*_repos"))
                       "~"))))

(eval-and-compile
  (defun fwoar-git-repo (name ssh-remote http-remote)
    (let ((dir-name (file-name-as-directory (expand-file-name name *fwoar-git-repos*))))
      (unless (file-exists-p dir-name)
        (ecase fwoar-git-mode
          (:ssh (magit-run-git-with-input "clone" ssh-remote dir-name))
          (:http (magit-run-git-with-input "clone" http-remote dir-name))))
      dir-name)))

(defvar *dotfiles-repo*
  (fwoar-git-repo "dotfiles"
                  "git@git.fiddlerwoaroof.com:dotfiles.git"
                  "https://git.fiddlerwoaroof.com/git/dotfiles.git"))

(defun fwoar/setup-load-path ()
  (let* ((new-load-path (cl-adjoin "~/.emacs.d/lisp/configurations/"
                                   load-path
                                   :test 'equal))
         (new-load-path (cl-adjoin (concat *dotfiles-repo*
                                           "emacs.d/lisp/configurations/")
                                   new-load-path
                                   :test 'equal))
         (new-load-path (cl-adjoin (concat *dotfiles-repo*
                                           "emacs.d/packages/")
                                   new-load-path
                                   :test 'equal)))
    (setq load-path new-load-path)))

(fwoar/setup-load-path)

(use-package fwoar-pastebin :ensure nil
  :custom
  (fwoar-pastebin-tramp-url (when (file-exists-p "~/.pastebin-name")
                              (car (read-sexps-in-file "~/.pastebin-name"))))
  (fwoar-pastebin-web-url-pattern (when (file-exists-p "~/.pastebin-name")
                                    (cadr (read-sexps-in-file "~/.pastebin-name")))))

(use-package fwoar-json-navigator
  :init (require 'fwoar-json-navigator)
  :ensure nil)

(defun fwoar/package-configuration (package)
  (fwoar/setup-load-path)
  (let* ((local-configs)
         (git-configs (concat *dotfiles-repo*
                              "emacs.d/lisp/configurations/"))
         (conf-file (concat (symbol-name package) "-conf.el"))
         (load-path (list* local-configs git-configs load-path)))
    conf-file))

(defun load-package-configuration (package)
  (let ((conf-file (fwoar/package-configuration package)))
    (load conf-file)))

(defmacro define-obsolete-function-alias ( obsolete-name current-name &optional when docstring)
  (declare (doc-string 4) (indent defun))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,(or when "unspecified"))))

(defun fwoar/load-local-packages ()
  (interactive)
  (mapc 'package-install-file
        (directory-files (format "%s/%s" *dotfiles-repo* "emacs.d/packages/")
                         t ".*[.]el$")))

(use-package json-mode
  :ensure t)
(unless (package-installed-p 'fwoar-functional-utils)
  (fwoar/load-local-packages))

(defvar fwoar-git-mode :ssh)
(when (locate-library "site-lisp")
  (load "site-lisp"))

(load-package-configuration 'lsp)



(fwoar/zenburn-with-color-variables
  (defface magit-keyword-feature
    `((t :foreground ,zenburn-green :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-chore
    `((t :foreground ,zenburn-blue :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-test
    `((t :foreground ,zenburn-blue-1 :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-refactor
    `((t :foreground ,zenburn-green+1 :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-misc
    `((t :foreground ,zenburn-fg-1 :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-bug
    `((t :foreground ,zenburn-red :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces))

(defvar fwoar::*magit-log-regexp-faces*
  '((magit-keyword-feature
     "^\\(\\(?:feat\\(?:ure\\)?(\\([^)]+?\\))\\)\\|\\(?:feat\\(ure\\)?\\>\\)\\)")
    (magit-keyword-chore "^\\(\\(?:chore(\\([^)]+?\\))\\)\\|\\(?:chore\\>\\)\\)")
    (magit-keyword-test "^\\(\\(?:test(\\([^)]+?\\))\\)\\|\\(?:test\\>\\)\\)")
    (magit-keyword-refactor "^\\(\\(?:refactor(\\([^)]+?\\))\\)\\|\\(?:refactor\\>\\)\\)")
    (magit-keyword-bug "^\\(\\(?:bug(\\([^)]+?\\))\\)\\|\\(?:bug\\>\\)\\)")
    (magit-keyword-test "^\\(\\(?:test(\\([^)]+?\\))\\)\\|\\(?:test\\>\\)\\)")
    ))

(defun fwoar/propertize-magit-log (_rev msg)
  (cl-loop for (face regexp) in fwoar::*magit-log-regexp-faces*
           do (let ((boundary 0))
                (while (string-match regexp msg boundary)
                  (setq boundary (match-end 0))
                  (magit--put-face (match-beginning 0) boundary face msg))))

  (let ((boundary 0))
    (while (string-match "^\\([^:\n\t]+\\):"  msg boundary)
      (setq boundary (match-end 0))
      (let ((group (match-string 1 msg)))
        (unless (or (> (length group) 20)
                    (s-starts-with? "feat" group)
                    (s-starts-with? "Merge" group)
                    (s-starts-with? "merge" group)
                    (s-starts-with? "chore" group)
                    (s-starts-with? "bug" group))
          (magit--put-face (match-beginning 0) (1- boundary)
                           'magit-keyword-misc msg))))))

(use-package magit
  :ensure t
  :config
  ;; TODO: figure this out with transients
  ;;(magit-define-popup-action 'magit-dispatch-popup ?j "Browse remote" 'browse-at-remote)
  'magit-dispatch

  (advice-add 'magit-log-propertize-keywords :after
              'fwoar/propertize-magit-log))

(use-package browse-at-remote
  :ensure t
  :custom
  (browse-at-remote-prefer-symbolic nil)

  :config
  (cl-pushnew '("git.fiddlerwoaroof.com$" . "gitlist")
              browse-at-remote-remote-type-regexps
              :test 'equal)

  (defun browse-at-remote--format-region-url-as-gitlist
      (repo-url ref relname start-line end-line)
    (unless (s-ends-with-p ".git" repo-url)
      (setf repo-url (format "%s.git" repo-url)))
    ;; gitlist doesn't support end-line
    (format "%s/blob/%s/%s#L%s" repo-url ref relname start-line))
  )


(use-package aggressive-indent :ensure t)
(load-package-configuration 'evil)
(load-package-configuration 'helm)
;; (load-package-configuration 'projectile)

;; slime depends on fwoar-git-repo
(load-package-configuration 'slime)
;; (load-package-configuration 'cider)

(global-company-mode 1)



(defun fwoar/c-a-p ()
  (interactive)
  (save-excursion
    (cl-destructuring-bind (start . _end) (bounds-of-thing-at-point 'defun)
      (goto-char start)
      (sp-wrap-with-pair "(")
      (insert "comment\n")
      (sp-indent-defun ))))


(comment
 (use-package multifiles
   :config
   (evil-define-key 'visual 'global (kbd "<leader>m") 'mf/mirror-region-in-multifile)))

(use-package http
  :ensure t)
(use-package graphql
  :ensure t)
(use-package ob-graphql
  :ensure t)
(use-package ob-http
  :ensure t)
(use-package ob-restclient
  :ensure t)

(defun safe-files ()
  (let ((fn (expand-file-name "~/.safe-files")))
    (when (file-exists-p fn)
      (read-strings-in-file fn))))

(defun fwoar/mark-safe (fn)
  (interactive (list buffer-file-name))
  (with-temp-buffer
    (insert "\n")
    (insert fn)
    (append-to-file (point-min) (point-max)
                    (expand-file-name "~/.safe-files"))))

(defvar-local safe-file-p nil)
(setf (get 'safe-file-p 'risky-local-variable) t)

(defun fwoar/confirm-babel-evaluate (lang body)
  (message "Buffer file name: %s" buffer-file-name)
  (let ((result (or safe-file-p
                    (member buffer-file-name (safe-files)))))
    (setq-local safe-file-p result)
    (not safe-file-p)))

(setq org-confirm-babel-evaluate 'fwoar/confirm-babel-evaluate)

(use-package org
  :pin "gnu"
  :ensure t
  :config
  (setq org-directory "~/org"
        org-default-notes-file (concat org-directory "/scratch.org")
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-log-done 'time
        org-log-into-drawer t
        org-capture-templates '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
                                 "* TODO %?\n  %i\n  %a")
                                ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
                                 "* %?\nEntered on %U\n  %i\n  %a")
                                ("s" "Snippet" entry (file "~/org/snippets.org")
                                 "* %?\n#+BEGIN_SRC\n%i\n#+END_SRC")
                                ("b" "Bookmarks" entry (file+olp+datetree "~/org/bookmarks.org")
                                 "* %? %^g\n%c\n")
                                ("a" "Agenda" entry
                                 (file "~/org/agenda.org")
                                 "* %? %^G\n  SCHEDULED: %T"))
        org-refile-targets '((nil . (:maxlevel . 2))))

  (defvar fwoar::*lob* (f-join org-directory "lob.org"))
  (org-babel-lob-ingest fwoar::*lob*)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t)
     (graphql . t)
     (http . t)
     (emacs-lisp . t)
     (lisp . t)
     (haskell . t)
     (shell . t)))

  (define-key global-map "\C-cc" 'org-capture)
  (evil-define-key 'visual 'global (kbd "<leader>c") 'org-capture))

(use-package delight
  :ensure t)

(use-package deadgrep
  :ensure t)

(use-package deft
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "<leader>v") 'deft))

(use-package emmet-mode
  :ensure t
  :config
  (define-key evil-insert-state-map (kbd "C-c ,") 'emmet-expand-line))

(use-package lisp-skeletons
  :config
  (add-hook 'skeleton-end-hook 'skeleton-make-markers)

  (define-key evil-insert-state-map (kbd "C-c j") 'skeleton-next-position)
  (define-key evil-insert-state-map (kbd "C-c k") 'skeleton-prev-position)
  (evil-define-key 'normal 'global (kbd "<leader>g") 'helm-generate-lisp-skeleton)
  (evil-define-key 'visual 'global (kbd "<leader>g") 'helm-generate-lisp-skeleton))


(use-package htmlize
  :ensure t)

(load-package-configuration 'javascript)

(use-package direnv
  :ensure t
  :config
  (direnv-mode 1)
  (add-hook 'js2-mode-hook 'direnv-mode)
  (add-hook 'typescript-mode-hook 'direnv-mode))


(use-package cl-generic
  :ensure t)

(defun intercept-print (f)
  (print f)
  f)

(cl-defgeneric fwoar--pl-selector ()
  (:method () (slime-selector)))
(cl-defgeneric fwoar--find-system ())

(defun fwoar-pl-selector ()
  (interactive)
  (fwoar--pl-selector))
(evil-define-key 'normal 'global (kbd "<leader>o") 'fwoar-pl-selector)


(use-package imenu
  :config
  (evil-define-key 'normal 'global (kbd "<leader>d") 'helm-imenu-in-all-buffers))

(defun fwoar--read-register-name ()
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


(use-package cl-format :ensure t)

(use-package eldoc :delight)

(use-package css-eldoc :ensure t)

(use-package csv-mode :ensure t)

(use-package eldoc-eval :ensure t)

(use-package highlight-parentheses
  :ensure t
  :delight
  :config
  (global-highlight-parentheses-mode 1))



(defun fwoar/markdown-mode-hook ()
  (setf left-margin-width 10
        right-margin-width 10))
(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'variable-pitch-mode)
  (add-hook 'markdown-mode-hook 'fwoar/markdown-mode-hook))

(use-package rainbow-delimiters :ensure t)

(use-package ripgrep :ensure t)

(use-package scss-mode :ensure t)

(use-package smartparens :ensure t :ensure t :config
  (sp-pair "${" "}")
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))
  (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode))

(use-package web-mode :ensure t)

(use-package yaml-mode :ensure t)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all)
  )


(use-package editorconfig
  :ensure t
  :delight (editorconfig-mode " ec")
  :config (editorconfig-mode 1))

(defun fwoar--activate-treemacs ()
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (delete-window)
    (treemacs-select-window)))

(defun fwoar--no-line-numbers ()
  (display-line-numbers-mode -1))

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-is-never-other-window t
        treemacs-no-png-images t)
  (global-set-key (kbd "s-e") 'fwoar--activate-treemacs)
  (global-set-key (kbd "s-1") 'fwoar--activate-treemacs)
  (add-hook 'treemacs-mode-hook 'fwoar--no-line-numbers)
  )

(use-package treemacs-evil
  :after treemacs evil
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
(when (file-exists-p custom-file)
  (load-file custom-file))

;;(setq gc-cons-threshold (* 100 1024))

(unless fwoar.is-ordinary
  (setq with-editor-emacsclient-executable "/usr/local/bin/emacsclient")
  (require 'cjpad)
  (find-file "~/org/notes.org"))

(put 'narrow-to-region
     'disabled
     nil)

(global-set-key (kbd "C-x C-b")
                'ibuffer)

(setq org-agenda-files '("~/org/notes.org" "~/org/agenda.org"))

(defvar url-pattern (when (file-exists-p "~/.pastebin-name")
                      (car (read-sexps-in-file "~/.pastebin-name"))))

(defun delete-mru-window ()
  (interactive)
  (delete-window
   (get-mru-window nil nil t)))

(add-to-list 'browse-url-filename-alist
             '("/Users/\\([^/]+\\)/\\(Sites\\|public_html\\)\\(/.*\\)" . "https://localhost/~\\1\\3"))

(progn ;; narrowing
  (define-key global-map "\C-c]" "\C-u1\C-xnp")
  (define-key global-map "\C-c[" "\C-u-1\C-xnp\M-<"))


(defun fwoar/zenburn-css ()
  (interactive)
  (mapcar (lambda (desc)
            (cl-destructuring-bind (name . value) desc
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

(comment
 (use-package org-brain :ensure t
   :init
   (setq org-brain-path "~/org-brain/")

   :config
   (eval-after-load 'evil
     '(evil-set-initial-state 'org-brain-visualize-mode 'emacs))

   (push '("B" "Brain" plain (function org-brain-goto-end)
           "* %i%?" :empty-lines 1)
         org-capture-templates)

   (setq org-id-track-globally t
         org-id-locations-file "~/.emacs.d/.org-id-locations"
         org-brain-visualize-default-choices 'all
         org-brain-title-max-length 0)))

(setq diary-file (expand-file-name "~/diary"))

(cl-defmacro fwoar/binding (setter target &body bindings)
  (declare (indent 2))
  (let ((target-sym (gensym)))
    `(let ((,target-sym ,target))
       ,(cons 'progn
              (mapcar (lambda (binding)
                        `(,setter ,target-sym ',(car binding) ,@(cdr binding)))
                      bindings)))))

(defun make-info-window ()
  (setq mode-line-format nil)
  ;;(centaur-tabs-local-mode 1)
  (set-window-dedicated-p (selected-window) t)
  (when-let (w (window-in-direction 'above))
    (set-window-parameter w 'no-delete-other-windows t))
  (fwoar/binding set-window-parameter (selected-window)
    (no-other-window t)
    (no-delete-other-windows t))
  ())

(comment
 (use-package prolog)
 (use-package ediprolog
   :after prolog
   :ensure t
   :config
   (define-key prolog-mode-map (kbd "C-j") 'ediprolog-dwim)))

(progn
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
(put 'list-timers 'disabled nil)

(defun fwoar/center-defun ()
  (interactive)
  (cl-destructuring-bind (a . b) (bounds-of-thing-at-point 'defun)
    (save-excursion
      (let ((s (progn (goto-char a) (line-number-at-pos)))
            (e (progn (goto-char b) (line-number-at-pos))))
        (evil-scroll-line-to-center (+ s -1 (ceiling (- e s) 2)))))))

(use-package which-key-posframe
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-posframe-mode 1))

(use-package nix-mode
  :ensure t)

(define-derived-mode zsh-mode sh-mode "ZSH" "Mode for zsh scripts"
  (setq-local sh-shell "zsh"))
