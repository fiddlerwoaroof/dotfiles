(defun fwoar--paste-register-from-helm-current-buffer (register)
  (interactive (fwoar--read-register-name))
  (let ((filename (with-current-buffer helm-current-buffer
                    (if (equal register
                               (elt (kbd "C-w") 0))
                        (word-at-point)
                      (evil-get-register register t)))))
    (when filename
      (insert filename))))

(use-package helm :ensure t
  :custom
  (helm-boring-buffer-regexp-list
   '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\Pfuture"))
  :delight

  :config
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (evil-define-key 'normal 'global (kbd "<leader>j") 'helm-buffers-list)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'helm-occur)
  (define-key helm-map (kbd "C-r") 'fwoar--paste-register-from-helm-current-buffer)
  (define-key helm-map (kbd "<right>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<left>") 'helm-find-files-up-one-level))

(use-package helm-ls-git :after helm :ensure t)
(use-package helm-org :after helm :ensure t)
(use-package helm-rg :after helm evil :ensure t
  :custom
  (helm-rg-default-directory 'git-root)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>S") 'helm-rg)
  )

(use-package fwoar-helm-project
  :init
  (require 'fwoar-helm-project)
  (fwoar::initialize-fwoar-helm-project)
  :no-require t
  :ensure nil
  )

(comment
 (use-package helm-ag :after helm :ensure t)

 (use-package helm-ag-r :after helm :ensure t)

 (use-package helm-css-scss :after helm :ensure t)

 (use-package project-explorer :after helm :ensure t))

(use-package helm-dash
  :ensure t
  :config)
