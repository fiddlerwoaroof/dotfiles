(defun fwoar/cider-hook-base ()
  (flycheck-mode)
  (rainbow-delimiters-mode 1)
  (evil-smartparens-mode 1)
  (smartparens-strict-mode 1)
  (helm-cider-mode 1)
  (cider-company-enable-fuzzy-completion))
(defun fwoar/cider-hook ()
  (fwoar/cider-hook-base)
  (aggressive-indent-mode 1))
(defun fwoar/cider-repl-hook ()
  (fwoar/cider-hook-base)
  (aggressive-indent-mode 0))

(defun fwoar/cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (with-current-buffer (cider-current-repl nil t)
      (let ((fw/window (get-buffer-window)))
        (with-selected-window fw/window
          (end-of-buffer)
          (insert form)
          (cider-repl-return)
          (end-of-buffer))))))

(use-package cider
  :ensure t
  :config
  (require 'cider-selector)
  (define-key evil-normal-state-map " t" 'cider-test-run-ns-tests)
  (evil-define-key 'normal clojure-mode-map " '" 'helm-cider-apropos)

  (def-cider-selector-method ?S "find clojure project file"
    (fwoar--find-system))

  (add-hook 'cider-mode-hook 'fwoar/cider-hook)
  (add-hook 'cider-repl-mode-hook 'fwoar/cider-repl-hook)

  (modify-syntax-entry ?: "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?_ "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?- "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?~ "w" clojure-mode-syntax-table)
  (modify-syntax-entry ?. "w" clojure-mode-syntax-table)

  (define-key cider-repl-mode-map (kbd "C-c M-o") 'cider-repl-clear-buffer)
  (define-key cider-repl-mode-map (kbd "C-c C-o") 'cider-repl-clear-output)
  (setq cider-save-file-on-load t
        cider-repl-history-file "~/.emacs.d/cider-history.clj")

  (define-key cider-mode-map
    (kbd "C-c C-j") 'fwoar/cider-eval-expression-at-point-in-repl))

(use-package flycheck-clj-kondo
  :ensure t)


