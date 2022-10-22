(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000
      load-prefer-newer t
      inhibit-splash-screen t
      inhibit-startup-message t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(message invocation-name)

(package-install 's)
(set-exec-path-from-shell-PATH)
(cold-boot)

(defun fwoar/setup-load-path ()
  (let* ((new-load-path (cl-adjoin (expand-file-name "~/.emacs.d/lisp/configurations/")
                                   load-path
                                   :test 'equal))
         (new-load-path (cl-adjoin (concat *dotfiles-repo*
                                           "emacs.d/lisp/configurations/")
                                   new-load-path
                                   :test 'equal))
         (new-load-path (cl-adjoin (concat *dotfiles-repo*
                                           "emacs.d/packages/")
                                   new-load-path
                                   :test 'equal)))
    (setq load-path new-load-path)))

(fwoar/setup-load-path)


(defun fwoar/package-configuration (package)
  (fwoar/setup-load-path)
  (let* ((local-configs)
         (git-configs (concat *dotfiles-repo*
                              "emacs.d/lisp/configurations/"))
         (conf-file (concat (symbol-name package) "-conf.el"))
         (load-path (list* local-configs git-configs load-path)))
    conf-file))

(defun load-package-configuration (package)
  (let ((conf-file (fwoar/package-configuration package)))
    (load conf-file)))

(defun fwoar/load-local-packages ()
  (interactive)
  (mapc 'package-install-file
        (directory-files (format "%s/%s" *dotfiles-repo* "emacs.d/packages/")
                         t ".*[.]el")))
