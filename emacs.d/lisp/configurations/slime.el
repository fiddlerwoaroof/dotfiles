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

(defun setup-lisp-mode ()
  (unless (string= "*slime-scratch*" (buffer-name))
    (smartparens-strict-mode 1)
    (evil-smartparens-mode 1)
    (aggressive-indent-mode 1))

  (define-key evil-insert-state-map "^N" 'slime-fuzzy-indent-and-complete-symbol)
  (rainbow-delimiters-mode))

(add-hook 'lisp-mode-hook 'setup-lisp-mode) 
(add-hook 'emacs-lisp-mode-hook 'setup-lisp-mode) 

(evil-define-key 'normal lisp-mode-map "gd" 'slime-edit-definition)

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

(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
(modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?\} "){" lisp-mode-syntax-table)

(setq shr-inhibit-images t
      shr-use-fonts nil)

(defun fwoar--clhs-lookup (&rest args)
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup (word-at-point))))

(pushnew (list ?h "Check hyperspec" #'fwoar--clhs-lookup)
         slime-selector-methods
         :key #'car)

(cl-defgeneric fwoar--find-system ())
(cl-defmethod fwoar--find-system (&context (major-mode lisp-mode))
  (let ((systems (directory-files
                  (locate-dominating-file default-directory
                                          (lambda (n)
                                            (directory-files n nil "^[^.#][^#]*[.]asd$")))
                  t "^[^.#][^#]*[.]asd$")))
    (find-file (if (not (null (cdr systems)))
                   (helm-comp-read "system:" systems)
                 (car systems)))))

(pushnew (list ?S "Goto System" #'fwoar--find-system)
         slime-selector-methods
         :key #'car)

(defun slime-ecl ()
  (interactive)
  (let ((inferior-lisp-program "ecl")
        (slime-lisp-implementations nil))
    (slime)))

(defun slime-cmucl ()
  (interactive)
  (let ((inferior-lisp-program "cmucl")
        (slime-lisp-implementations nil))
    (slime)))

(defun slime-sbcl ()
  (interactive)
  (let ((inferior-lisp-program "sbcl")
        (slime-lisp-implementations nil))
    (slime)))

(defun slime-ccl ()
  (interactive)
  (let ((inferior-lisp-program "ccl")
        (slime-lisp-implementations nil))
    (slime)))

(defun find-use-clause (current-form)
  (when current-form
    (destructuring-bind (discriminator . packages) current-form
      (case discriminator
        (:use (remove-if (op (or (eql :cl _)))
                         (cdr current-form)))
        (defpackage (find-use-clause
                     (find-if (lambda (f)
                                (and (listp f)
                                     (eql (car f) :use)))
                              '(defpackage :tracking-sim (:use :cl :alexandria :serapeum) (:export)))))))))

(defun load-package-uses ()
  (interactive)
  (slime-eval-async `(ql:quickload ',(find-use-clause (list-at-point)))))


(message (format "s-c-c is: %s" slime-company-completion))

(setq slime-contribs '(slime-fancy
                       slime-company
                       slime-macrostep
                       slime-trace-dialog
                       slime-mdot-fu
                       slime-buffer-streams
                       slime-indentation)
      slime-export-save-file t)

(slime-setup)

;;;;; }}}

(defmacro define-lisp-implementations (&rest decl)
  `(progn
     ,@(cl-loop for (symbol . args) in decl
                collect `(progn
                           (defun ,symbol ()
                             (interactive)
                             (slime ',symbol))
                           (cl-pushnew '(,symbol ,@args) slime-lisp-implementations
                                       :key 'car)))))

(with-eval-after-load "slime"
  (when (or (eq system-type 'gnu/linux)
            (eq system-type 'darwin))
    (define-lisp-implementations
      (abcl  ("abcl"))
      (ccl ("ccl"))
      (clisp ("clisp"))
      (cmucl ("cmucl" "-quiet"))
      (ecl   ("ecl"))
      ;;(mkcl  ("mkcl"))
      ;;(xcl   ("xcl"))
      (sbcl  ("sbcl" "--dynamic-space-size" "8192")))))


(defun sp-absorb-forward-sexp (&optional arg)
  "Absorb previous expression.

Save the expressions preceding point and delete them.  Then slurp
an expression backward and insert the saved expressions.

With ARG positive N, absorb that many expressions.

Examples:

​  (do-stuff 1)         (save-excursion
​  (save-excursion  ->   |(do-stuff 1)
​   |(do-stuff 2))        (do-stuff 2))

  foo bar (concat |baz quux) -> (concat |foo bar baz quux) ;; 2"
  (interactive "p")
  (sp-forward-whitespace)
  (let* ((old (point))
         (raise (progn
                  (sp-end-of-sexp)
                  (buffer-substring (point) old))))
    (delete-region old (point))
    (sp-forward-slurp-sexp arg)
    (sp-backward-whitespace)
    (sp-end-of-sexp)
    (insert raise)
    (save-excursion
      (sp-backward-up-sexp)
      (indent-sexp)))
  (sp-backward-whitespace))
