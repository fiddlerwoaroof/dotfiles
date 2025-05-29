;; (setq-default no-native-compile t)
;; (setq native-comp-deferred-compilation nil)
;; (require 'cl-generic)
;; (cl-defgeneric (setf seq-elt) (store sequence n))

;; (setq-default native-comp-driver-options
;;               '("-Wl,-w"
;;                 "-Wl,-L/nix/store/zl3aslw7dhrk6wb5nv960hnc2v27l3j5-libgccjit-14-20241116/lib/gcc/aarch64-apple-darwin/14.2.1"
;;                 "-Wl,-L/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib"))


(setq package-user-dir
      (locate-user-emacs-file
       (concat
        (file-name-as-directory "elpa")
        emacs-version)))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(let ((my-theme-path (expand-file-name "~/.emacs.d/themes/")))
  (add-to-list 'load-path my-theme-path)
  (add-to-list 'custom-theme-load-path my-theme-path)
  (load-theme 'fwoar-zenburn t))

(fwoar:zenburn-with-color-variables
 (modify-all-frames-parameters
  `(
    (top . 701)
    (left . 1288)
    (width . 195)
    (height . 59)
    (vertical-scroll-bars . nil)
    (right-divider-width . 2)
    (bottom-divider-width . 2)
    (frame-resize-pixelwise . t)
    ;; (inhibit-double-buffering . t)
    (ns-appearance . dark)
    (ns-transparent-titlebar . t)
    (cursor-color . ,zenburn-fg+1)
    )))
