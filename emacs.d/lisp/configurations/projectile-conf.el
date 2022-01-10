(use-package projectile
  :ensure t
  :config
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

  (define-key evil-normal-state-map "gf" 'project-aware-ffap)
  (projectile-mode 1)
  )

(use-package org-projectile
  :ensure t
  :after projectile company
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

(use-package projectile-ripgrep
  :ensure t
  :after projectile)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package org-projectile-helm
  :ensure t
  :after org-projectile helm helm-org
  :config
  (define-key projectile-mode-map (kbd "C-c n p") 'org-projectile-helm-template-or-project))

(use-package helm-projectile
  :after helm projectile
  :ensure t
  :config

  (evil-define-key 'normal 'global (kbd "<leader>f") 'helm-projectile)
  (evil-define-key 'normal 'global (kbd "<leader>S") 'helm-projectile-rg)
  (evil-define-key 'normal 'global (kbd "<leader>h") 'helm-projectile-find-file-dwim))

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

(defun edit-init-el ()
  (interactive)
  (let ((default-directory *dotfiles-repo*))
    (helm-projectile-find-file)))

(defun helm-projectile-rg ()
  "Projectile version of `helm-rg'."
  (interactive)
  (if (require 'helm-rg nil t)
      (if (projectile-project-p)
          (let ((helm-rg-prepend-file-name-line-at-top-of-matches nil)
                (helm-rg-include-file-on-every-match-line t))
            (helm-rg (helm-projectile-rg--region-selection)
                     nil
                     (list (projectile-project-root))))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-rg' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-rg)
            (helm-projectile-rg))
        (error "`helm-rg' is not available.  Is MELPA in your `package-archives'?")))))
