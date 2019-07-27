(defun wrap-with-doublequote (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))


(defun wrap-in-dollar-brace
    (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "${"))

(use-package evil
  :ensure t
  :config

  (define-key evil-motion-state-map (kbd "C-w C-o") 'delete-mru-window)
  (define-key evil-motion-state-map (kbd "C-w C-w") 'evil-window-mru)

  (define-key evil-normal-state-map "ZZ" 'save-buffer)

  (advice-add 'evil-delete-marks :after
              (lambda (&rest args)
                (evil-visual-mark-render)))

  (define-key evil-insert-state-map (kbd "TAB") 'company-indent-or-complete-common)
  (evil-mode)


  (progn ;; navigation
    (define-key evil-normal-state-map " f" 'helm-projectile)
    (define-key evil-normal-state-map " j" 'helm-buffers-list)
    (define-key evil-normal-state-map " u" 'undo-tree-visualize))

  (progn ;; completion
    (define-key evil-normal-state-map (kbd "TAB") 'company-indent-or-complete-common)
    (define-key evil-insert-state-map (kbd "TAB") 'company-indent-or-complete-common))

  (progn ;; workaround until this fixed: https://github.com/emacs-evil/evil/issues/1129
    ;;                          or this: https://github.com/emacs-evil/evil/pull/1130
    (defun config/fix-evil-window-move (orig-fun &rest args)
      "Close Treemacs while moving windows around."
      (if (fboundp 'treemacs-get-local-window)
          (let* ((treemacs-window (treemacs-get-local-window))
                 (is-active (and treemacs-window (window-live-p treemacs-window))))
            (when is-active (treemacs))
            (apply orig-fun args)
            (when is-active
              (save-selected-window
                (treemacs))))
        (apply orig-fun args)))

    (dolist (func '(evil-window-move-far-left
                    evil-window-move-far-right
                    evil-window-move-very-top
                    evil-window-move-very-bottom))
      (advice-add func
                  :around #'config/fix-evil-window-move)))
  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-leader
  :after evil
  :config (evil-leader/set-leader "ß"))

(use-package evil-smartparens
  :ensure t
  :after evil smartparens
  :config
  (evil-smartparens-mode 1)

  (progn ;; wrapping
    (define-key evil-normal-state-map ",W" 'sp-wrap-round)
    (define-key cider-mode-map (kbd "C-, W") 'sp-wrap-round)
    (define-key cider-mode-map (kbd "C-, C-W") 'sp-wrap-round)

    (define-key evil-normal-state-map ",w(" 'sp-wrap-round)
    (define-key evil-normal-state-map ",w)" 'sp-wrap-round)
    (define-key cider-mode-map (kbd "C-, w (") 'sp-wrap-round)
    (define-key cider-mode-map (kbd "C-, w )") 'sp-wrap-round)

    (define-key evil-normal-state-map ",w$" 'wrap-in-dollar-brace)

    (define-key evil-normal-state-map ",w{" 'sp-wrap-curly)
    (define-key evil-normal-state-map ",w}" 'sp-wrap-curly)
    (define-key cider-mode-map (kbd "C-, w {") 'sp-wrap-curly)
    (define-key cider-mode-map (kbd "C-, w }") 'sp-wrap-curly)

    (define-key evil-normal-state-map ",w[" 'sp-wrap-square)
    (define-key evil-normal-state-map ",w]" 'sp-wrap-square)
    (define-key cider-mode-map (kbd "C-, w [") 'sp-wrap-square)
    (define-key cider-mode-map (kbd "C-, w ]") 'sp-wrap-square)

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
        centaur-tabs-adjust-buffer-order 'left)
  (centaur-tabs-enable-buffer-reordering)

  (define-key evil-normal-state-map "gt" 'centaur-tabs-forward-tab)
  (define-key global-map (kbd "<header-line> <wheel-up>") 'centaur-tabs-forward-tab)
  (define-key global-map (kbd "s-}") 'centaur-tabs-forward-tab)
  (define-key evil-normal-state-map "gT" 'centaur-tabs-backward-tab)
  (define-key global-map (kbd "<header-line> <wheel-down>") 'centaur-tabs-backward-tab)
  (define-key global-map (kbd "s-{") 'centaur-tabs-backward-tab))
