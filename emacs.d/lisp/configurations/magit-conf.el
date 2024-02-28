(fwoar:zenburn-with-color-variables
  (defface magit-keyword-feature
    `((t :foreground ,zenburn-green :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-chore
    `((t :foreground ,zenburn-blue :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-misc
    `((t :foreground ,zenburn-fg-1 :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces)
  (defface magit-keyword-bug
    `((t :foreground ,zenburn-red :inherit magit-keyword))
    "Face for parts of commit messages inside brackets."
    :group 'magit-faces))

(defun fwoar/propertize-magit-log (_rev msg)
  (let ((boundary 0))
    (while (string-match "^\\(\\(?:feat\\(?:ure\\)?(\\([^)]+?\\))\\)\\|\\(?:feat\\(ure\\)?\\>\\)\\)" msg boundary)
      (setq boundary (match-end 0))
      (magit--put-face (match-beginning 0) boundary
                       'magit-keyword-feature msg)))
  (let ((boundary 0))
    (while (string-match "^\\(\\(?:chore(\\([^)]+?\\))\\)\\|\\(?:chore\\>\\)\\)" msg boundary)
      (setq boundary (match-end 0))
      (magit--put-face (match-beginning 0) boundary
                       'magit-keyword-chore msg)))
  (let ((boundary 0))
    (while (string-match "^\\(\\(?:bug(\\([^)]+?\\))\\)\\|\\(?:bug\\>\\)\\)"  msg boundary)
      (setq boundary (match-end 0))
      (magit--put-face (match-beginning 0) boundary
                       'magit-keyword-bug msg)))
  (let ((boundary 0))
    (while (string-match "^\\([^:\n\t]+\\):"  msg boundary)
      (setq boundary (match-end 0))
      (let ((group (match-string 1 msg)))
        (unless (or (> (length group) 20)
                    (s-starts-with? "feat" group)
                    (s-starts-with? "Merge" group)
                    (s-starts-with? "merge" group)
                    (s-starts-with? "chore" group)
                    (s-starts-with? "bug" group))
          (magit--put-face (match-beginning 0) (1- boundary)
                           'magit-keyword-misc msg))))))

(use-package magit
  :ensure t
  :config
  ;; TODO: figure this out with transients
  ;;(magit-define-popup-action 'magit-dispatch-popup ?j "Browse remote" 'browse-at-remote)
  'magit-dispatch

  (advice-add 'magit-log-propertize-keywords :after
              'fwoar/propertize-magit-log))

(use-package browse-at-remote
  :after magit
  :ensure t
  :config)


(cl-defun fwoar::get-forge-projects (&optional (org "*") (repo "*") (forge "github.com"))
  (mapcar 'f-dirname
          (remove-if-not 'f-dir-p
                         (f-glob (f-join *fwoar-git-repos*
                                         forge
                                         org
                                         repo
                                         ".git")))))


(defun fwoar:switch-to-gh-project (project)
  (interactive (list (completing-read "project? "
                                      (fwoar::get-forge-projects))))
  (project-switch-project (message project)))
