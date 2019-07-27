(comment
 (use-package paredit
   :ensure t
   :config
   (defparedit-wrapper back-then-wrap paredit-wrap-sexp)
   (defparedit-wrapper back-then-wrap-square paredit-wrap-square)
   (defparedit-wrapper back-then-wrap-curly paredit-wrap-curly)
   (defparedit-wrapper back-then-wrap-angled paredit-wrap-angled)
   (defparedit-wrapper back-then-wrap-doublequote paredit-meta-doublequote)
   (define-key evil-normal-state-map ",W" 'back-then-wrap)
   (define-key evil-normal-state-map ",w]" 'back-then-wrap-square)
   (define-key evil-normal-state-map ",w}" 'back-then-wrap-curly)
   (define-key evil-normal-state-map ",w>" 'back-then-wrap-angled)
   (define-key evil-normal-state-map ",w\"" 'back-then-wrap-doublequote)

   (define-key evil-normal-state-map ",S" 'paredit-splice-sexp)
   (define-key evil-normal-state-map ",A" 'paredit-splice-sexp-killing-backward)
   (define-key evil-normal-state-map ",D" 'paredit-splice-sexp-killing-forward)
   (define-key evil-normal-state-map ",|" 'paredit-split-sexp)
   (define-key evil-normal-state-map ",J" 'paredit-join-sexps)
   (define-key evil-normal-state-map ",<" 'paredit-backward-slurp-sexp)
   (define-key evil-normal-state-map ",," 'paredit-backward-barf-sexp) 
   (define-key evil-normal-state-map ",>" 'paredit-forward-slurp-sexp)
   (define-key evil-normal-state-map ",." 'paredit-forward-barf-sexp) 
   (define-key evil-normal-state-map ",~" 'paredit-convolute-sexp))

 (use-package erc
   :config
   (add-hook 'erc-insert-post-hook 'erc-truncate-buffer))

 (use-package ansi-term :no-require t
   :config
   (eval-after-load 'evil
     (evil-set-initial-state 'term-mode 'emacs)))

 (defun ansi-term-post (&rest r)
   (message "Loading ansi term...")
   (evil-set-initial-state 'term-mode 'emacs))

 (advice-add 'ansi-term :after 'ansi-term-post))
