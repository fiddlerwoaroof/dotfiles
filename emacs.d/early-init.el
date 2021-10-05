(require 'cl-generic)
(cl-defgeneric (setf seq-elt) (store sequence n))

(setq default-frame-alist
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
