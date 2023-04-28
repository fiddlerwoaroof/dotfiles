;;;; SLIME SETUP {{{
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

(add-to-list 'load-path
             (fwoar-git-repo "3dp/slime/"
                             "git@github.com:slime/slime.git"
                             "https://github.com/slime/slime.git"))
(add-to-list 'load-path
             (fwoar-git-repo "3dp/slime-company/"
                             "git@github.com:anwyn/slime-company.git"
                             "https://github.com/anwyn/slime-company.git"))
;; (require 'slime)


;; put slime-company in load-path
;; (require 'slime-company)

(defmacro define-lisp-implementations (&rest decl)
  `(progn
     ,@(cl-loop for (symbol . args) in decl
                collect `(progn
                           (defun ,symbol ()
                             (interactive)
                             (slime ',symbol))
                           (cl-pushnew '(,symbol ,@args) slime-lisp-implementations
                                       :key 'car)))))

(defun setup-lisp-mode ()
  (unless (string= "*slime-scratch*" (buffer-name))
    (smartparens-strict-mode 1)
    (evil-smartparens-mode 1)
    (aggressive-indent-mode 1))

  (define-key evil-insert-state-map "^N" 'slime-fuzzy-indent-and-complete-symbol)
  (rainbow-delimiters-mode))

(defun fwoar--clhs-lookup (&rest args)
  (let ((browse-url-browser-function 'eww-browse-url))
    (hyperspec-lookup (word-at-point))))

(defun fwoar--get-asds ()
  (let ((dir-of-asd (locate-dominating-file default-directory
                                            (lambda (n)
                                              (directory-files n nil "^[^.#][^#]*[.]asd$")))))
    (when dir-of-asd
      (directory-files dir-of-asd
                       t "^[^.#][^#]*[.]asd$"))))

(cl-defmethod fwoar--find-system (&context (major-mode lisp-mode))
  (let ((systems (fwoar--get-asds)))
    (find-file (if (not (null (cdr systems)))
                   (helm-comp-read "system:" systems)
                 (car systems)))))

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

(defun slime-lw ()
  (interactive)
  (let ((inferior-lisp-program "lw")
        (slime-lisp-implementations nil))
    (slime)))

(defun slime-ccl ()
  (interactive)
  (let ((inferior-lisp-program "ccl")
        (slime-lisp-implementations nil))
    (slime)))

(defun find-use-clause (current-form)
  (when current-form
    (cl-destructuring-bind (discriminator . packages) current-form
      (cl-case discriminator
        (:use (remove-if (op (or (eql :cl _)))
                         (cdr current-form)))
        (defpackage (find-use-clause
                     (find-if (lambda (f)
                                (and (listp f)
                                     (eql (car f) :use)))
                              '(defpackage :tracking-sim
                                 (:use :cl :alexandria :serapeum)
                                 (:export)))))))))

(defun load-package-uses ()
  (interactive)
  (slime-eval-async `(ql:quickload ',(find-use-clause (list-at-point)))))

;;(message (format "s-c-c is: %s" slime-company-completion))

;;;;; }}}

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


(use-package slime-company
  :ensure nil
  :no-require t
  :config
  (setq slime-company-completion 'fuzzy)
  )

(defvar passwords ())

(use-package slime
  :ensure nil
  :after smartparens
  :config
  (lexical-let ((hyperspec-regexp "\\(HyperSpec\\)\\|\\(CLHS\\)"))
    ;; eww customization
    (defun fwoar::find-h1 (tree &optional found)
      (if (or (null tree)
              found)
          tree
        (if (eq (car tree) 'h1)
            nil
          (cons (if (consp (car tree))
                    (fwoar::find-h1 (car tree))
                  (car tree))
                (if (consp (cdr tree))
                    (fwoar::find-h1 (cdr tree))
                  (cdr tree))))))


    (defun fwoar::cleanup-hyperspec ()
      (when (s-match hyperspec-regexp
                     (plist-get eww-data :url))
        (eww-display-html nil nil
                          (fwoar::find-h1 (plist-get eww-data :dom))
                          nil
                          (current-buffer))))

    (add-hook 'eww-after-render-hook 'fwoar::cleanup-hyperspec)
    (add-hook 'eww-after-render-hook 'eww-readable)
    (customize-set-variable 'browse-url-handlers
                            `((,hyperspec-regexp . eww-browse-url)
                              ("newadvent.org/cathen" . eww-browse-url)))
    (customize-set-variable
     'common-lisp-hyperspec-root
     "file:///Applications/LispWorks%208.0%20(64-bit)/Library/lib/8-0-0-0/manual/html-m/CLHS/")
    (values)
    )

  (defslimefun get-passwd (id prompt)
    (let ((val (assoc id passwords)))
      (cdr
       (if val val
         (car (push (cons id (read-passwd prompt))
                    passwords))))))

  (message "%s" load-path)
  (when (or (eq system-type 'gnu/linux)
            (eq system-type 'darwin))
    (define-lisp-implementations
     (abcl  ("abcl"))
     (lw  ("lw"))
     (ccl ("ccl"))
     (clisp ("clisp"))
     (cmucl ("cmucl" "-quiet"))
     (ecl   ("ecl"))
     ;;(mkcl  ("mkcl"))
     ;;(xcl   ("xcl"))
     (sbcl  ("sbcl" "--dynamic-space-size" "8192"))
     ))

  (global-set-key (kbd "C-c x") 'slime-export-symbol-at-point)

  (when (and (boundp 'common-lisp-hyperspec-root)
             (string-prefix-p "/" common-lisp-hyperspec-root))
    (setq common-lisp-hyperspec-root
          (concat "file://" common-lisp-hyperspec-root)))

  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "~/.nix-profile/bin/sbcl")

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

  (pushnew (list ?h "Check hyperspec" #'fwoar--clhs-lookup)
           slime-selector-methods
           :key #'car)

  (pushnew (list ?S "Goto System" #'fwoar--find-system)
           slime-selector-methods
           :key #'car)

  (setq slime-contribs '(slime-media
                         slime-fancy
                         slime-company
                         slime-macrostep
                         slime-trace-dialog
                         slime-mdot-fu
                         slime-buffer-streams
                         slime-indentation
                         slime-asdf)
        slime-export-save-file t)

  (slime-setup slime-contribs)

  (defslime-repl-shortcut fwoar--slime-repl-load-asd ("load-asd")
                          (:handler (lambda ()
                                      (interactive)
                                      (let ((system-files (fwoar--get-asds)))
                                        (slime-eval-async (cons 'cl:progn
                                                                (mapcar (lambda (it)
                                                                          `(cl:progn (asdf:load-asd ,it) ,it))
                                                                        system-files))
                                          (lambda (r)
                                            (message "Loading ASDs done: %s" r))))))
                          (:one-liner "Load asd for current project"))


  (defadvice slime-eval-last-expression-in-repl
      (around fwoar/seleir/activate-old-window activate)
    (let ((fwoar/old-window (selected-window)))
      (unwind-protect (progn ad-do-it)
        (select-window fwoar/old-window))))

  (comment (defslime-repl-shortcut fwoar--slime-repl-quickload ("quickload")
                                   (:handler (lambda ()
                                               (interactive)
                                               (let ((system-files (fwoar--get-asds)))
                                                 (slime-eval-async (cons 'cl:progn
                                                                         (mapcar (lambda (it)
                                                                                   `(cl:progn (asdf:load-asd ,it) ,it))
                                                                                 system-files))
                                                   (lambda (r)
                                                     (message "Loading ASDs done: %s" r))))))
                                   (:one-liner "Load asd for current project"))))
