#+fw.dump
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :uiop))

#+fw.dump
(ql:quickload '(:net.didierverna.clon :alexandria))

(defpackage :fwoar.zenburn
  (:use :cl )
  (:export #:dump
           #:html-color
           #:rgb-color))
(in-package :fwoar.zenburn)

(defun 256-color-text (fg bg format &rest args)
  (cond ((or fg bg)
         (format T "~c[~:[~;~:*38;2;~{~d;~}~]~:[~;~:*48;2;~{~d;~}~]m~?~@*~c[39m~:*~c[49m"
                 #\Esc
                 fg
                 bg
                 format
                 args))
        (t (error "must specify either fg or bg for a color"))))

(defparameter *color-alist*
  '((fg+2     . (#xFF #xFF #xEF))
    (fg+1     . (#xF5 #xF5 #xD6))
    (fg       . (#xDC #xDC #xCC))
    (fg-1     . (#xA6 #xA6 #x89))
    (fg-2     . (#x65 #x65 #x55))
    (black    . (#x00 #x00 #x00))
    (bg-2     . (#x00 #x00 #x00))
    (bg-1     . (#x11 #x11 #x12))
    (bg-05    . (#x38 #x38 #x38))
    (bg       . (#x2A #x2B #x2E))
    (bg+05    . (#x49 #x49 #x49))
    (bg+1     . (#x4F #x4F #x4F))
    (bg+2     . (#x5F #x5F #x5F))
    (bg+3     . (#x6F #x6F #x6F))
    (red+2    . (#xEC #xB3 #xB3))
    (red+1    . (#xDC #xA3 #xA3))
    (red      . (#xCC #x93 #x93))
    (red-1    . (#xBC #x83 #x83))
    (red-2    . (#xAC #x73 #x73))
    (red-3    . (#x9C #x63 #x63))
    (red-4    . (#x8C #x53 #x53))
    (red-5    . (#x7C #x43 #x43))
    (red-6    . (#x6C #x33 #x33))
    (orange   . (#xDF #xAF #x8F))
    (yellow   . (#xF0 #xDF #xAF))
    (yellow-1 . (#xE0 #xCF #x9F))
    (yellow-2 . (#xD0 #xBF #x8F))
    (green-5  . (#x2F #x4F #x2F))
    (green-4  . (#x3F #x5F #x3F))
    (green-3  . (#x4F #x6F #x4F))
    (green-2  . (#x5F #x7F #x5F))
    (green-1  . (#x6F #x8F #x6F))
    (green    . (#x7F #x9F #x7F))
    (green+1  . (#x8F #xB2 #x8F))
    (green+2  . (#x9F #xC5 #x9F))
    (green+3  . (#xAF #xD8 #xAF))
    (green+4  . (#xBF #xEB #xBF))
    (cyan     . (#x93 #xE0 #xE3))
    (blue+3   . (#xBD #xE0 #xF3))
    (blue+2   . (#xAC #xE0 #xE3))
    (blue+1   . (#x94 #xBF #xF3))
    (blue     . (#x8C #xD0 #xD3))
    (blue-1   . (#x7C #xB8 #xBB))
    (blue-2   . (#x6C #xA0 #xA3))
    (blue-3   . (#x5C #x88 #x8B))
    (blue-4   . (#x4C #x70 #x73))
    (blue-5   . (#x36 #x60 #x60))
    (magenta  . (#xDC #x8C #xC3))))

(defun theme-color (name)
  (cdr (assoc name *color-alist*)))

(defun html-color (name &optional (s t))
  (let ((values (theme-color name)))
    (prog1 (format s "#~{~2,'0x~}" values)
      (unless (null s)
        (format s "~%")))))

(defmacro may ((op arg &rest r))
  (let ((cond (case op
                (cl:funcall (car r))
                (t arg))))
    (alexandria:once-only (arg)
      `(when ,cond
         (,op ,arg ,@r)))))

(defun rgb-color (name &optional (float t))
  (let* ((lookup (find-symbol (string name) :fwoar.zenburn))
         (color (may (theme-color lookup))))
    (cond ((and color float)
           (mapcar (lambda (it)
                     (/ it 255d0))
                   color))
          (color))))

(defun zenburn-text (fg bg text &rest format-args)
  (let ((fgcolor (when fg (cdr (assoc fg *color-alist* :test 'equal))))
        (bgcolor (when bg (cdr (assoc bg *color-alist* :test 'equal)))))
    (apply #'256-color-text fgcolor bgcolor text format-args)))

(defun summary ()
  (loop for (color . values) in *color-alist*
        do
           (zenburn-text () color (make-string 32 :initial-element #\space))
           (format t "  ~8<~a~> (~{~2x~^, ~}) ~:* (~{~3d~^, ~})~%" color values)))

#+fw.dump
(defvar *synopsis*
  (net.didierverna.clon:defsynopsis (:postfix "[TEXT...]" :make-default nil)
    (flag :short-name "h" :long-name "help")
    (enum :short-name "f" :long-name "fg" :enum (mapcar 'car *color-alist*)
          :description "Set the text's foreground color")
    (enum :short-name "b" :long-name "bg" :enum (mapcar 'car *color-alist*)
          :description "Set the text's background color")
    (enum :long-name "html"  :enum (mapcar 'car *color-alist*)
          :description "Show COLOR as an HTML RGB literal")
    (enum :long-name "css"  :enum (mapcar 'car *color-alist*)
          :description "Show COLOR as an CSS RGB literal")))

#+fw.dump
(defun main ()
  (let* ((context (net.didierverna.clon:make-context :synopsis *synopsis*))
         (net.didierverna.clon:*context* context)
         (foreground (net.didierverna.clon:getopt :context context
                                                  :long-name "fg"))
         (background (net.didierverna.clon:getopt :context context
                                                  :long-name "bg"))
         (remainder (net.didierverna.clon:remainder :context context))
         (css (net.didierverna.clon:getopt :context context
                                           :long-name "css"))
         (html (net.didierverna.clon:getopt :context context
                                            :long-name "html")))
    (cond ((net.didierverna.clon:getopt :context context
                                        :long-name "help")
           (net.didierverna.clon:help))
          ((and html css)
           (format *error-output* "Can't use HTML and CSS options together~%")
           (net.didierverna.clon:help))
          (css
           (let ((values (cdr (assoc css *color-alist*))))
             (format t "rgb(~{~d~^, ~})~%" values)))
          (html
           (html-color html t))
          ((null remainder)
           (summary))
          ((or foreground background)
           (zenburn-text foreground background "~{~a~^ ~}" remainder))
          (t
           (net.didierverna.clon:help)))))

#+fw.dump
(defun dump ()
  (setf net.didierverna.clon:*context* nil
        *features* (remove :fw.dump *features*)
        *print-case* :downcase)
  (net.didierverna.clon:dump "zenburn" main))
