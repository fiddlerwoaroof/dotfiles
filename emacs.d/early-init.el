(require 'cl-generic)
(cl-defgeneric (setf seq-elt) (store sequence n))

(modify-all-frames-parameters
 '(
   (top . 701)
   (left . 1288)
   (width . 195)
   (height . 59)
   (vertical-scroll-bars . nil)
   (right-divider-width . 2)
   (bottom-divider-width . 2)
   ;; (inhibit-double-buffering . t)
   (ns-appearance . dark)
   (ns-transparent-titlebar . t)
   ))

(setq package-user-dir
      (locate-user-emacs-file
       (concat
        (file-name-as-directory "elpa")
        emacs-version)))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
