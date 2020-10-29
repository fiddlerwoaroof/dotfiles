
(use-package helm :ensure t :config
  (require 'helm-config)
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'helm-projectile)
  (evil-define-key 'normal 'global (kbd "<leader>j") 'helm-buffers-list)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'helm-occur)
  (evil-define-key 'normal 'global (kbd "<leader>S") 'helm-projectile-rg)
  (define-key helm-map (kbd "C-r") 'fwoar--paste-register-from-helm-current-buffer)
  (define-key helm-map (kbd "<right>") 'helm-execute-persistent-action)
  (define-key helm-map
    (kbd "<left>")
    'helm-find-files-up-one-level))

(use-package helm-org
  :after helm
  :ensure t)

(use-package org-projectile-helm
  :after helm
  :ensure t
  :after org-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c n p") 'org-projectile-helm-template-or-project))

(use-package helm-ag :after helm :ensure t)

(use-package helm-ag-r :after helm :ensure t)

(use-package helm-rg :after helm :ensure t)

(use-package helm-css-scss :after helm :ensure t)

(use-package helm-ls-git :after helm :ensure t)

(use-package helm-projectile :after helm :ensure t
             :config
  (evil-define-key 'normal 'global (kbd "<leader>h") 'helm-projectile-find-file-dwim)
  )

(use-package project-explorer :after helm :ensure t)
