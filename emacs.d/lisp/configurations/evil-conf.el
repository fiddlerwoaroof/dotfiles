(defun wrap-with-doublequote (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))


(defun wrap-in-dollar-brace
    (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "${"))

(defun fwoar-delete-mru-window ()
  (interactive)
  (delete-window
   (get-mru-window nil nil t)))

(use-package evil
  :ensure t
  :config

  (evil-define-key 'motion 'global  (kbd "TAB") nil)
  (evil-define-key 'motion 'global  (kbd "C-w C-o") 'fwoar-delete-mru-window)
  (evil-define-key 'motion 'global  (kbd "C-w C-w") 'evil-window-mru)

  (evil-define-key 'normal 'global (kbd "ZZ") 'save-buffer)

  (advice-add 'evil-delete-marks :after
              (lambda (&rest args)
                (evil-visual-mark-render)))

  (evil-define-key 'insert 'global  (kbd "TAB") 'company-indent-or-complete-common)
  (evil-mode)

  (evil-set-leader '(normal) (kbd "<SPC>"))

  (progn ;; navigation
    (evil-define-key 'normal 'global (kbd "<leader>f") 'helm-projectile)
    (evil-define-key 'normal 'global (kbd "<leader>;") 'helm-semantic-or-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>j") 'helm-buffers-list)
    (evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize))

  (progn ;; completion
    (evil-define-key 'normal company-mode-map (kbd "TAB") 'company-indent-or-complete-common)
    (evil-define-key 'insert company-mode-map (kbd "TAB") 'company-indent-or-complete-common))

  (evil-define-key 'normal 'global (kbd "<leader>a") 'magit)

  (progn ;; error jumping
    (evil-define-key 'motion 'global (kbd "]e") 'flycheck-next-error)
    (evil-define-key 'motion 'global (kbd "[e") 'flycheck-previous-error))

  ;; make evil keybindings apply right away when I open a new buffer
  (add-hook 'window-configuration-change-hook 'evil-normalize-keymaps))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-smartparens
  :ensure t
  :delight
  :after evil smartparens
  :config
  (evil-smartparens-mode 1)

  (progn ;; wrapping
    (evil-define-key 'normal 'global (kbd ",W") 'sp-wrap-round)
    (evil-define-key 'normal 'global  (kbd "C-, W") 'sp-wrap-round)
    (evil-define-key 'normal 'global  (kbd "C-, C-W") 'sp-wrap-round)

    (evil-define-key 'normal 'global (kbd ",w(") 'sp-wrap-round)
    (evil-define-key 'normal 'global (kbd ",w)") 'sp-wrap-round)
    (define-key global-map (kbd "C-, (") 'sp-wrap-round)
    (define-key global-map (kbd "C-, C-(") 'sp-wrap-round)
    (define-key global-map (kbd "C-, )") 'sp-wrap-round)
    (define-key global-map (kbd "C-, C-)") 'sp-wrap-round)

    (evil-define-key 'normal 'global (kbd ",w$") 'wrap-in-dollar-brace)
    (evil-define-key 'normal 'global  (kbd "C-, $") 'wrap-in-dollar-brace)
    (evil-define-key 'normal 'global (kbd ",w{") 'sp-wrap-curly)
    (evil-define-key 'normal 'global (kbd ",w}") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, {") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, C-{") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, }") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, C-}") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, w {") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, w }") 'sp-wrap-curly)

    (evil-define-key 'normal 'global (kbd ",w[") 'sp-wrap-square)
    (evil-define-key 'normal 'global (kbd ",w]") 'sp-wrap-square)
    (define-key global-map (kbd "C-, w [") 'sp-wrap-square)
    (define-key global-map (kbd "C-, <escape>") 'sp-wrap-square)
    (define-key global-map (kbd "C-, [") 'sp-wrap-square)
    (define-key global-map (kbd "C-, w ]") 'sp-wrap-square)
    (define-key global-map (kbd "C-, C-]") 'sp-wrap-square)
    (define-key global-map (kbd "C-, ]") 'sp-wrap-square)

    (evil-define-key 'normal 'global (kbd ",w\"") 'wrap-with-doublequote)
    (define-key cider-mode-map (kbd "C-, w \"") 'sp-wrap-doublequote))

  (progn ;; splicing
    (evil-define-key 'normal 'global (kbd ",S") 'sp-splice-sexp)
    (define-key cider-mode-map (kbd "C-, S") 'sp-splice-sexp)
    (define-key cider-mode-map (kbd "C-, C-S") 'sp-splice-sexp)
    (evil-define-key 'normal 'global (kbd ",A") 'sp-splice-sexp-killing-backward)
    (define-key cider-mode-map (kbd "C-, A") 'sp-splice-sexp-killing-backward)
    (define-key cider-mode-map (kbd "C-, C-A") 'sp-splice-sexp-killing-backward)
    (evil-define-key 'normal 'global (kbd ",D") 'sp-splice-sexp-killing-forward)
    (define-key cider-mode-map (kbd "C-, D") 'sp-splice-sexp-killing-forward)
    (define-key cider-mode-map (kbd "C-, C-D") 'sp-splice-sexp-killing-forward)
    (evil-define-key 'normal 'global (kbd ",F") 'sp-splice-sexp-killing-around)
    (define-key cider-mode-map (kbd "C-, F") 'sp-splice-sexp-killing-around)
    (define-key cider-mode-map (kbd "C-, C-F") 'sp-splice-sexp-killing-around))

  (progn ;; barf/slurp
    (evil-define-key 'normal 'global (kbd ",,") 'sp-backward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, ,") 'sp-backward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, C-,") 'sp-backward-barf-sexp)
    (evil-define-key 'normal 'global (kbd ",.") 'sp-forward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, .") 'sp-forward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, C-.") 'sp-forward-barf-sexp)
    (evil-define-key 'normal 'global (kbd ",<") 'sp-backward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, <") 'sp-backward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, C-<") 'sp-backward-slurp-sexp)
    (evil-define-key 'normal 'global (kbd ",>") 'sp-forward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, >") 'sp-forward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, C->") 'sp-forward-slurp-sexp))

  (progn ;; misc
    (evil-define-key 'normal 'global (kbd ",~") 'sp-convolute-sexp)
    (evil-define-key 'normal 'global (kbd ",a") 'sp-absorb-sexp)
    (evil-define-key 'normal 'global (kbd ",e") 'sp-emit-sexp)
    (evil-define-key 'normal 'global (kbd ",`") 'sp-clone-sexp)
    (evil-define-key 'normal 'global (kbd ",J") 'sp-join-sexp)
    (evil-define-key 'normal 'global (kbd ",|") 'sp-split-sexp))

  (progn ;; narrowing
    (evil-define-key 'normal 'global (kbd "<leader>n(") 'sp-narrow-to-sexp)
    (evil-define-key 'normal 'global (kbd "<leader>n)") 'sp-narrow-to-sexp)
    (evil-define-key 'normal 'global (kbd "<leader>nn") 'narrow-to-defun)
    (evil-define-key 'normal 'global (kbd "<leader>nr") 'narrow-to-region)
    (evil-define-key 'normal 'global (kbd "<leader>nw") 'widen)))

(use-package centaur-tabs
  :ensure t
  :after evil
  :config
  (setq centaur-tabs-adjust-buffer-order t
        centaur-tabs-adjust-buffer-order 'right)
  (centaur-tabs-enable-buffer-reordering)

  (evil-define-key 'normal 'global (kbd "gt") 'centaur-tabs-forward-tab)
  (define-key global-map (kbd "<header-line> <wheel-up>") 'centaur-tabs-forward-tab)
  (define-key global-map (kbd "s-}") 'centaur-tabs-forward-tab)
  (evil-define-key 'normal 'global (kbd "gT") 'centaur-tabs-backward-tab)
  (define-key global-map (kbd "<header-line> <wheel-down>") 'centaur-tabs-backward-tab)
  (define-key global-map (kbd "s-{") 'centaur-tabs-backward-tab))


(defun setup-special-mode ()
  (company-mode -1))

(add-hook 'special-mode-hook
          'setup-special-mode)
