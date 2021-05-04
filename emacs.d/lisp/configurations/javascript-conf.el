(use-package js2-mode
  :ensure t
  :defer t
  :commands js2-mode
  :config
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js2-mode-map (kbd "M-.") nil)
  (modify-syntax-entry ?_ "w" js2-mode-syntax-table)
  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
  (setq-default js2-basic-offset 4)
  (setq-default js-indent-level 4)
  (define-key js2-mode-map (kbd "<leader>t") 'fwoar/trigger-jest)
  (add-hook 'js2-mode-hook 'flycheck-mode)
  (customize-set-variable 'js2-mode-show-parse-errors nil)
  (customize-set-variable 'js2-strict-missing-semi-warning nil)
  (customize-set-variable 'js2-strict-trailing-comma-warning nil)
  (customize-set-variable 'js2-strict-inconsistent-return-warning nil))

(use-package js
  :ensure t
  :config
  (modify-syntax-entry ?_ "w" js-mode-syntax-table)

  ;;; indent ternaries with arrow function correctly---
  (defun js--looking-at-operator-p ()
    "Return non-nil if point is on a JavaScript operator, other than a comma."
    (save-match-data
      (and (looking-at js--indent-operator-re)
           (or (not (eq (char-after) ?:))
               (save-excursion
                 (js--backward-syntactic-ws)
                 (when (memq (char-before) '(?\) ?})) (backward-list))
                 (and (js--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                      (eq (char-after) ??))))
           (not (and
                 (eq (char-after) ?/)
                 (save-excursion
                   (eq (nth 3 (syntax-ppss)) ?/))))
           (not (and
                 (eq (char-after) ?*)
                 ;; Generator method (possibly using computed property).
                 (looking-at (concat "\\* *\\(?:\\[\\|" js--name-re " *(\\)"))
                 (save-excursion
                   (js--backward-syntactic-ws)
                   ;; We might misindent some expressions that would
                   ;; return NaN anyway.  Shouldn't be a problem.
                   (memq (char-before) '(?, ?} ?{)))))))))

(defun fwoar/typescript-mode-hook ()
  (tree-sitter-require 'typescript)
  (when (s-suffix-p ".tsx" buffer-file-name)
    (setq-local tree-sitter-language (tree-sitter-require 'tsx)))
  (flycheck-mode 1)
  (lsp)
  (prettier-js-mode 1)
  (tree-sitter-mode 1)
  (tree-sitter-hl-mode 1)
  (comment
   (tide-setup)
   (tide-hl-identifier-mode 1))
  )

(use-package typescript-mode
  :ensure t
  :config
  (define-key typescript-mode-map (kbd "<leader>t") 'fwoar/trigger-jest)
  (add-hook 'typescript-mode-hook 'fwoar/typescript-mode-hook)
  (add-to-list 'auto-mode-alist
               '("\\.tsx$" . typescript-mode)))

(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :after tree-sitter
  :ensure t)

(comment
 (use-package tide
   :ensure t
   :config
   (add-hook 'js2-mode-hook 'tide-setup)
   (add-hook 'typescript-mode-hook 'fwoar/typescript-mode-hook)
   (add-hook 'js2-mode-hook 'tide-hl-identifier-mode)
   (comment
    (flycheck-add-next-checker 'javascript-eslint
                               'javascript-tide
                               'append))))

(use-package rjsx-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'lsp)
  (define-key rjsx-mode-map (kbd "M-.") nil)
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode)))

(comment
 (use-package tern
   :config
   (add-hook 'js-mode-hook (lambda () (tern-mode t)))
   (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

 (use-package company-tern
   :ensure t
   :config
   (add-to-list 'company-backends 'company-tern)
   (setq company-tooltip-align-annotations t)))

(use-package jest
  :ensure t
  :config
  (defun jest--project-root ()
    "Find the project root directory."
    (let ((closest-package-json (fwoar--find-package-json))
          (projectile-root (projectile-project-root)))
      (message "%s <-> %s" closest-package-json projectile-root)
      (if (s-prefix-p projectile-root closest-package-json)
          closest-package-json
        projectile-root))))

(comment
 (use-package vue-mode
   :ensure t
   :config
   (add-hook 'vue-mode
             'prettier-js-mode)
   (add-hook 'vue-mode
             'flycheck-mode)))

(use-package prettier-js
  :ensure t
  :delight " p"
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'css-mode 'prettier-js-mode))

(cl-defgeneric fwoar/test-on-save ()
  (:method ()))

(defvar-local fwoar/*test-file-name* nil)
(defun fwoar/trigger-jest ()
  (interactive)
  (when-let ((test-name (if fwoar/*test-file-name*
                            fwoar/*test-file-name*
                          (setq-local fwoar/*test-file-name*
                                      (if (projectile-test-file-p buffer-file-name)
                                          buffer-file-name
                                        (projectile-find-implementation-or-test buffer-file-name))))))
    (let ((proc (make-network-process
                 :name "jest-comm"
                 :buffer "*jest-comm*"
                 :family 'local
                 :service "/tmp/jest.sock")))
      (unwind-protect
          (process-send-string proc test-name)
        (delete-process proc)))))

(cl-defmethod fwoar/test-on-save (&context (major-mode (derived-mode typescript-mode)))
  (fwoar/trigger-jest))
(cl-defmethod fwoar/test-on-save (&context (major-mode (derived-mode js-mode)))
  (fwoar/trigger-jest))

(defvar *tos-hook*
  (add-hook 'after-save-hook 'fwoar/test-on-save))
