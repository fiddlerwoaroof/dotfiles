(defun wrap-with-doublequote (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))

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
  (use-package evil-smartparens
    :ensure t
    :after smartparens
    :config
    (evil-smartparens-mode 1)


    (progn ;; wrapping
      (define-key evil-normal-state-map ",W" 'sp-wrap-round)
      (define-key evil-normal-state-map ",w(" 'sp-wrap-round)
      (define-key evil-normal-state-map ",w)" 'sp-wrap-round)

      (define-key evil-normal-state-map ",w{" 'sp-wrap-curly)
      (define-key evil-normal-state-map ",w}" 'sp-wrap-curly)

      (define-key evil-normal-state-map ",w[" 'sp-wrap-square)
      (define-key evil-normal-state-map ",w]" 'sp-wrap-square)

      (define-key evil-normal-state-map ",w\"" 'wrap-with-doublequote))

    (progn ;; splicing
      (define-key evil-normal-state-map ",S" 'sp-splice-sexp)
      (define-key evil-normal-state-map ",A" 'sp-splice-sexp-killing-backward)
      (define-key evil-normal-state-map ",D" 'sp-splice-sexp-killing-forward)
      (define-key evil-normal-state-map ",F" 'sp-splice-sexp-killing-around))

    (progn ;; barf/slurp
      (define-key evil-normal-state-map ",," 'sp-backward-barf-sexp)
      (define-key evil-normal-state-map ",." 'sp-forward-barf-sexp)
      (define-key evil-normal-state-map ",<" 'sp-backward-slurp-sexp)
      (define-key evil-normal-state-map ",>" 'sp-forward-slurp-sexp))

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

  ;; (use-package  evil-paredit
  ;;   :ensure t
  ;;   :after paredit
  ;;   :config
  ;;   (evil-paredit-mode))


  ;;(use-package evil-numbers
  ;;  :ensure t
  ;;  :config
  ;;  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  ;;  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-leader
    :config (evil-leader/set-leader "ß")))

(use-package centaur-tabs
  :ensure t
  :config
  (define-key evil-normal-state-map "gt" 'centaur-tabs-forward-tab)
  (define-key evil-normal-state-map "gT" 'centaur-tabs-backward-tab)
  )
