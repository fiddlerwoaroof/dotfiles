(use-package lsp-mode
  :ensure t
  :config (setq lsp-enable-snippet t))

(defun fwoar/lsp-ui-hook ()
  (lsp-ui-sideline-mode -1))

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  :config
  (add-hook 'lsp-ui-mode-hook 'fwoar/lsp-ui-hook))

(use-package lsp-treemacs
  :after treemacs lsp
  :ensure t)

(comment
 (use-package lsp-treemacs
   :after lsp-mode treemacs
   :ensure t)

 (use-package maven-test-mode
   :ensure t
   :after general evil
   :hook java-mode
   :config
   (evil-define-key 'normal maven-test-mode-map
     (kbd "<leader>t") 'maven-test-all
     (kbd "<leader>T") 'maven-test-toggle-between-test-and-class-other-window))

 (use-package lsp-java
   :ensure t
   :hook (java-mode . lsp)))

(comment
 (use-package lsp-haskell
   :ensure t
   :config
   (setq lsp-haskell-process-path-hie "ghcide")
   (setq lsp-haskell-process-args-hie '())
   ;; Comment/uncomment this line to see interactions between lsp client/server.
   ;;(setq lsp-log-io t)
   ))
