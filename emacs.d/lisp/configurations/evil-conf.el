(defun fwoar:wrap-with-doublequote (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "\""))

(defun fwoar:wrap-in-dollar-brace
    (&optional arg)
  (interactive "P")
  (sp-wrap-with-pair "${"))

(defun fwoar-delete-mru-window ()
  (interactive)
  (delete-window
   (get-mru-window nil nil t)))

(setq evil-want-keybinding nil)

(use-package undo-fu
  :ensure t)

(use-package evil
  :ensure t
  :after undo-fu
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  :custom
  (evil-undo-system 'undo-fu)
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

  (evil-set-leader '(normal visual) (kbd "<SPC>"))

  (progn ;; navigation
    (evil-define-key 'normal 'global (kbd "<leader>f") 'fwoar::browse-project)
    (evil-define-key 'normal 'global (kbd "<leader>;") 'helm-semantic-or-imenu)
    (evil-define-key 'normal 'global (kbd "<leader>j") 'helm-buffers-list)
    (comment
     (evil-define-key 'normal 'global (kbd "<leader>u") 'undo-tree-visualize)))

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

(define-keymap :prefix 'fwoar:smartparens-map
  ;; wrapping
  "W" #'sp-wrap-round
  "C-W" #'sp-wrap-round
  "w" (define-keymap :prefix 'fwoar:smartparens-wrap-map
        "(" #'sp-wrap-round
        ")" #'sp-wrap-round

        "{" #'sp-wrap-curly
        "}" #'sp-wrap-curly

        "[" #'sp-wrap-square
        "]" #'sp-wrap-square

        "\"" #'fwoar:wrap-with-doublequote
        "$" #'fwoar:wrap-in-dollar-brace)

  ;; narrowing
  "n" (define-keymap :prefix 'fwoar:smartparens-narrow-map
        "(" #'sp-narrow-to-sexp
        ")" #'sp-narrow-to-sexp
        "d" #'narrow-to-defun
        "n" #'narrow-to-defun
        "r" #'narrow-to-region
        "w" #'widen)

  ;; splicing
  "S" #'sp-splice-sexp
  "A" #'sp-splice-sexp-killing-backward
  "D" #'sp-splice-sexp-killing-forward
  "F" #'sp-splice-sexp-killing-around

  ;; paren manipulation
  "," #'sp-backward-barf-sexp
  "." #'sp-forward-barf-sexp
  "<" #'sp-backward-slurp-sexp
  ">" #'sp-forward-slurp-sexp

  ;; misc

  "~" 'sp-convolute-sexp
  "a" 'sp-absorb-sexp
  "e" 'sp-emit-sexp
  "`" 'sp-clone-sexp
  "J" 'sp-join-sexp
  "|" 'sp-split-sexp)


(use-package evil-smartparens
  :ensure t
  :delight
  :after evil smartparens
  :config

  (evil-smartparens-mode 1)

  (define-key global-map (kbd "C-,") 'fwoar:smartparens-map)
  (evil-define-key 'normal 'global (kbd ",") 'fwoar:smartparens-map))

(defun fwoar/setup-evil-collection-for-mode (mode)
  (evil-collection-require mode)
  (lexical-let ((mode mode))
    (with-eval-after-load mode
      (funcall (intern (format "evil-collection-%s-setup" mode))))))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (fwoar/setup-evil-collection-for-mode 'eshell)
  (fwoar/setup-evil-collection-for-mode 'deadgrep)
  ;; Bad idea, messes with bindings too much :)
  ;; (fwoar/setup-evil-collection-for-mode 'magit)
  (fwoar/setup-evil-collection-for-mode 'org)
  (fwoar/setup-evil-collection-for-mode 'xref)
  )

(comment
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
   (define-key global-map (kbd "s-{") 'centaur-tabs-backward-tab)))


(defun setup-special-mode ()
  (company-mode -1))

(add-hook 'special-mode-hook
          'setup-special-mode)
