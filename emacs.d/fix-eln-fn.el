(defun fwoar:fix-eln-hook (eln-file)
  (message "NOTICE ME: fixing eln: %s" eln-file)
  (let ((clean-eln-name (expand-file-name "~/git_repos/dotfiles/emacs.d/clean-eln")))
    (call-process clean-eln-name nil nil nil eln-file)))
