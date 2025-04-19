
(defun fwoar:fix-eln-hook (eln-file)
  (message "NOTICE ME: fixing eln: %s" eln-file)
  (call-process "/Users/edwlan/emacs-fix/clean-eln"
                nil nil nil
                eln-file))
