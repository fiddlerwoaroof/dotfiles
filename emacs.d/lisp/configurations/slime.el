;;;; SLIME SETUP {{{
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(add-to-list 'load-path "~/git_repos/3dp/slime/")
(require 'slime)

(use-package slime-company
  :ensure t)

(global-set-key (kbd "C-c x") 'slime-export-symbol-at-point)

(when (and (boundp 'common-lisp-hyperspec-root)
           (string-prefix-p "/" common-lisp-hyperspec-root))
  (setq common-lisp-hyperspec-root
        (concat "file://" common-lisp-hyperspec-root)))

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "~/sbcl/bin/sbcl")

(add-hook 'lisp-mode-hook
          '(lambda ()
             ;;(define-key evil-insert-state-map "^N" 'slime-fuzzy-indent-and-complete-symbol)
             ;; (unless (string= "*slime-scratch*" (buffer-name))
             ;;   (paredit-mode)
             ;;   (evil-paredit-mode))
             (rainbow-delimiters-mode))) 

(setq slime-contribs
      '(slime-fancy
        slime-company
        slime-macrostep
        slime-trace-dialog
        slime-mdot-fu))


(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?* "w" lisp-mode-syntax-table)
(modify-syntax-entry ?+ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?! "w" lisp-mode-syntax-table)
(modify-syntax-entry ?$ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?% "w" lisp-mode-syntax-table)
(modify-syntax-entry ?& "w" lisp-mode-syntax-table)
(modify-syntax-entry ?% "w" lisp-mode-syntax-table)
(modify-syntax-entry ?= "w" lisp-mode-syntax-table)
(modify-syntax-entry ?< "w" lisp-mode-syntax-table)
(modify-syntax-entry ?> "w" lisp-mode-syntax-table)
(modify-syntax-entry 91 "(" lisp-mode-syntax-table)
(modify-syntax-entry 93 ")" lisp-mode-syntax-table)
;;(modify-syntax-entry ?@ "w" lisp-mode-syntax-table)

(modify-syntax-entry ?^ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?~ "w" lisp-mode-syntax-table)
(modify-syntax-entry ?. "w" lisp-mode-syntax-table)

(setq shr-inhibit-images t
      shr-use-fonts nil)

(defun fwoar--clhs-lookup (&rest args)
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup (word-at-point))))

(pushnew (list ?h "Check hyperspec" #'fwoar--clhs-lookup)
         slime-selector-methods
         :key #'car)

(defun fwoar--slime-find-system ()
  (let ((systems (directory-files
                  (locate-dominating-file default-directory
                                          (lambda (n)
                                            (directory-files n nil "^[^.#][^#]*[.]asd$")))
                  t "^[^.#][^#]*[.]asd$")))
    (find-file (if (not (null (cdr systems)))
                   (helm-comp-read "system:" systems)
                 (car systems)))))

(pushnew (list ?S "Goto System" #'fwoar--slime-find-system)
         slime-selector-methods
         :key #'car)




 ;;;;; }}}
