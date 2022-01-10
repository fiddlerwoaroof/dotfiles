(defun safe-files ()
  (let ((fn (expand-file-name "~/.safe-files")))
    (when (file-exists-p fn)
      (read-strings-in-file fn))))

(defun fwoar/mark-safe (fn)
  (interactive (list buffer-file-name))
  (with-temp-buffer
    (insert "\n")
    (insert fn)
    (append-to-file (point-min) (point-max)
                    (expand-file-name "~/.safe-files"))))

(defvar-local safe-file-p nil)
(setf (get 'safe-file-p 'risky-local-variable) t)

(defun fwoar/confirm-babel-evaluate (lang body)
  (message "Buffer file name: %s" buffer-file-name)
  (let ((result (or safe-file-p
                    (member buffer-file-name (safe-files)))))
    (setq-local safe-file-p result)
    (not safe-file-p)))

(use-package ob-graphql
  :ensure t)
(use-package ob-http
  :ensure t)
(use-package ob-restclient
  :ensure t)

(use-package org
  :ensure t
  :pin gnu
  :config
  (setq org-directory "~/org"
        org-confirm-babel-evaluate 'fwoar/confirm-babel-evaluate
        org-default-notes-file (concat org-directory "/scratch.org")
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-log-done 'time
        org-log-into-drawer t
        org-capture-templates '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
                                 "* TODO %?\n  %i\n  %a")
                                ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
                                 "* %?\nEntered on %U\n  %i\n  %a")
                                ("s" "Snippet" entry (file "~/org/snippets.org")
                                 "* %?\n#+BEGIN_SRC\n%i\n#+END_SRC")
                                ("b" "Bookmarks" entry (file+olp+datetree "~/org/bookmarks.org")
                                 "* %? %^g\n%c\n")
                                ("a" "Agenda" entry
                                 (file "~/org/agenda.org")
                                 "* %? %^G\n  SCHEDULED: %T"))
        org-refile-targets '((nil . (:maxlevel . 2))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t)
     (graphql . t)
     (http . t)
     (emacs-lisp . t)
     (lisp . t)
     (haskell . t)))

  (define-key global-map "\C-cc" 'org-capture)
  (evil-define-key 'visual 'global (kbd "<leader>c") 'org-capture))

(use-package org-projectile
  :ensure t
  :after org company
  :config
  (progn
    (org-projectile-per-project)
    (setq org-agenda-skip-unavailable-files t)
    (setq org-projectile-per-project-filepath
          "notes/README.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)
    (define-key projectile-mode-map (kbd "C-c c") 'org-capture))
  :ensure t)
