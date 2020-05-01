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

  (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-motion-state-map (kbd "C-w C-o") 'fwoar-delete-mru-window)
  (define-key evil-motion-state-map (kbd "C-w C-w") 'evil-window-mru)

  (define-key evil-normal-state-map "ZZ" 'save-buffer)

  (advice-add 'evil-delete-marks :after
              (lambda (&rest args)
                (evil-visual-mark-render)))

  (define-key evil-insert-state-map (kbd "TAB") 'company-indent-or-complete-common)
  (evil-mode)


  (progn ;; navigation
    (define-key evil-normal-state-map " f" 'helm-projectile)
    (define-key evil-normal-state-map " ;" 'helm-semantic-or-imenu)
    (define-key evil-normal-state-map " j" 'helm-buffers-list)
    (define-key evil-normal-state-map " u" 'undo-tree-visualize))

  (progn ;; completion
    (evil-define-key 'normal company-mode-map (kbd "TAB") 'company-indent-or-complete-common)
    (evil-define-key 'insert company-mode-map (kbd "TAB") 'company-indent-or-complete-common))

  (evil-define-key 'normal magit-file-mode-map " a" 'magit)

  (progn ;; error jumping
    (define-key evil-motion-state-map "]e" 'flycheck-next-error)
    (define-key evil-motion-state-map "[e" 'flycheck-previous-error))

  ;; make evil keybindings apply right away when I open a new buffer
  (add-hook 'window-configuration-change-hook 'evil-normalize-keymaps))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-leader
  :ensure t
  :after evil
  :config (evil-leader/set-leader "ß"))

(use-package evil-smartparens
  :ensure t
  :delight
  :after evil smartparens
  :config
  (evil-smartparens-mode 1)

  (progn ;; wrapping
    (define-key evil-normal-state-map ",W" 'sp-wrap-round)
    (define-key evil-normal-state-map (kbd "C-, W") 'sp-wrap-round)
    (define-key evil-normal-state-map (kbd "C-, C-W") 'sp-wrap-round)

    (define-key evil-normal-state-map ",w(" 'sp-wrap-round)
    (define-key evil-normal-state-map ",w)" 'sp-wrap-round)
    (define-key global-map (kbd "C-, (") 'sp-wrap-round)
    (define-key global-map (kbd "C-, C-(") 'sp-wrap-round)
    (define-key global-map (kbd "C-, )") 'sp-wrap-round)
    (define-key global-map (kbd "C-, C-)") 'sp-wrap-round)

    (define-key evil-normal-state-map ",w$" 'wrap-in-dollar-brace)
    (define-key evil-normal-state-map (kbd "C-, $") 'wrap-in-dollar-brace)
    (define-key evil-normal-state-map ",w{" 'sp-wrap-curly)
    (define-key evil-normal-state-map ",w}" 'sp-wrap-curly)
    (define-key global-map (kbd "C-, {") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, C-{") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, }") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, C-}") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, w {") 'sp-wrap-curly)
    (define-key global-map (kbd "C-, w }") 'sp-wrap-curly)

    (define-key evil-normal-state-map ",w[" 'sp-wrap-square)
    (define-key evil-normal-state-map ",w]" 'sp-wrap-square)
    (define-key global-map (kbd "C-, w [") 'sp-wrap-square)
    (define-key global-map (kbd "C-, <escape>") 'sp-wrap-square)
    (define-key global-map (kbd "C-, [") 'sp-wrap-square)
    (define-key global-map (kbd "C-, w ]") 'sp-wrap-square)
    (define-key global-map (kbd "C-, C-]") 'sp-wrap-square)
    (define-key global-map (kbd "C-, ]") 'sp-wrap-square)

    (define-key evil-normal-state-map ",w\"" 'wrap-with-doublequote)
    (define-key cider-mode-map (kbd "C-, w \"") 'sp-wrap-doublequote))

  (progn ;; splicing
    (define-key evil-normal-state-map ",S" 'sp-splice-sexp)
    (define-key cider-mode-map (kbd "C-, S") 'sp-splice-sexp)
    (define-key cider-mode-map (kbd "C-, C-S") 'sp-splice-sexp)
    (define-key evil-normal-state-map ",A" 'sp-splice-sexp-killing-backward)
    (define-key cider-mode-map (kbd "C-, A") 'sp-splice-sexp-killing-backward)
    (define-key cider-mode-map (kbd "C-, C-A") 'sp-splice-sexp-killing-backward)
    (define-key evil-normal-state-map ",D" 'sp-splice-sexp-killing-forward)
    (define-key cider-mode-map (kbd "C-, D") 'sp-splice-sexp-killing-forward)
    (define-key cider-mode-map (kbd "C-, C-D") 'sp-splice-sexp-killing-forward)
    (define-key evil-normal-state-map ",F" 'sp-splice-sexp-killing-around)
    (define-key cider-mode-map (kbd "C-, F") 'sp-splice-sexp-killing-around)
    (define-key cider-mode-map (kbd "C-, C-F") 'sp-splice-sexp-killing-around))

  (progn ;; barf/slurp
    (define-key evil-normal-state-map ",," 'sp-backward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, ,") 'sp-backward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, C-,") 'sp-backward-barf-sexp)
    (define-key evil-normal-state-map ",." 'sp-forward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, .") 'sp-forward-barf-sexp)
    (define-key cider-mode-map (kbd "C-, C-.") 'sp-forward-barf-sexp)
    (define-key evil-normal-state-map ",<" 'sp-backward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, <") 'sp-backward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, C-<") 'sp-backward-slurp-sexp)
    (define-key evil-normal-state-map ",>" 'sp-forward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, >") 'sp-forward-slurp-sexp)
    (define-key cider-mode-map (kbd "C-, C->") 'sp-forward-slurp-sexp))

  (progn ;; misc
    (define-key evil-normal-state-map ",~" 'sp-convolute-sexp)
    (define-key evil-normal-state-map ",a" 'sp-absorb-sexp)
    (define-key evil-normal-state-map ",e" 'sp-emit-sexp)
    (define-key evil-normal-state-map ",`" 'sp-clone-sexp)
    (define-key evil-normal-state-map ",J" 'sp-join-sexp)
    (define-key evil-normal-state-map ",|" 'sp-split-sexp))

  (progn ;; narrowing
    (define-key evil-normal-state-map " n(" 'sp-narrow-to-sexp)
    (define-key evil-normal-state-map " n)" 'sp-narrow-to-sexp)
    (define-key evil-normal-state-map " nn" 'narrow-to-defun)
    (define-key evil-normal-state-map " nr" 'narrow-to-region)
    (define-key evil-normal-state-map " nw" 'widen)))

(use-package centaur-tabs
  :ensure t
  :after evil
  :config
  (setq centaur-tabs-adjust-buffer-order t
        centaur-tabs-adjust-buffer-order 'right)
  (centaur-tabs-enable-buffer-reordering)

  (define-key evil-normal-state-map "gt" 'centaur-tabs-forward-tab)
  (define-key global-map (kbd "<header-line> <wheel-up>") 'centaur-tabs-forward-tab)
  (define-key global-map (kbd "s-}") 'centaur-tabs-forward-tab)
  (define-key evil-normal-state-map "gT" 'centaur-tabs-backward-tab)
  (define-key global-map (kbd "<header-line> <wheel-down>") 'centaur-tabs-backward-tab)
  (define-key global-map (kbd "s-{") 'centaur-tabs-backward-tab))


(defun setup-special-mode ()
  (company-mode -1))

(add-hook 'special-mode-hook
          'setup-special-mode)
