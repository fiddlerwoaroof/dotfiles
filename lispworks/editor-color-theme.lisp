;;;; Copyright (C) 2013 Paulo Madeira
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Interface

(cl:in-package #:cl-user)

(defpackage #:editor-color-theme
  (:use #:cl)
  (:export #:all-color-themes
           #:color-theme-args
           #:color-theme
           #:define-color-theme
           #:remove-color-theme
           #:zenburn-paren-colors))

(in-package #:editor-color-theme)

;;; Configuration

(defvar *foreground-color* nil)

(defvar *background-color* nil)

(defconstant +default-parenthesis-font-face-colours+ '(:red :black :darkgreen :darkorange3 :blue :purple))

;;; Implementation

(defvar *all-color-themes* (make-hash-table :test 'string=))

(defun all-color-themes ()
  (loop for key being the hash-keys in *all-color-themes*
        collect key))

(defun color-theme-data (theme-name)
  (multiple-value-bind (color-theme-data found?)
      (gethash theme-name *all-color-themes*)
    (if found?
        color-theme-data
        (error "No color theme named ~s found." theme-name))))

(defun color-theme-super-theme-names (theme-name)
  (first (color-theme-data theme-name)))

(defun color-theme-args (theme-name)
  (rest (color-theme-data theme-name)))

(defvar *all-editor-panes* (make-hash-table :test 'eq
                                            :weak-kind :key))

(defun update-editor-pane (pane)
  (setf (capi:simple-pane-foreground pane) (or *foreground-color* :color_windowtext))
  (setf (capi:simple-pane-background pane) (or *background-color* :color_window))

  (let ((recolorize-p (editor::buffer-font-lock-mode-p (capi:editor-pane-buffer pane))))
    (when recolorize-p
      (gp:invalidate-rectangle pane)))
  (values))

(defun update-editor-panes ()
  (maphash #'(lambda (pane value)
               (declare (ignore value))
               (update-editor-pane pane))
           *all-editor-panes*)
  (values))

(defvar *editor-face-names*
  '(:region
    :show-point-face
    :interactive-input-face
    :highlight
    :non-focus-complete-face
    :font-lock-function-name-face
    :font-lock-comment-face
    :font-lock-type-face
    :font-lock-variable-name-face
    :font-lock-string-face
    :font-lock-keyword-face
    :font-lock-builtin-face
    :compiler-note-highlight
    :compiler-warning-highlight
    :compiler-error-highlight
    ))

(defun set-color-theme (theme-name)
  (destructuring-bind (&rest color-theme-args
                       &key foreground background &allow-other-keys)
      (color-theme-args theme-name)

    (setf *foreground-color* (or foreground :color_windowtext))
    (setf *background-color* (or background :color_window))

    (lw:when-let (parenthesis-colors
                  (getf color-theme-args :parenthesis-font-face-colours
                        +default-parenthesis-font-face-colours+))
      (editor::set-parenthesis-colours parenthesis-colors))

    (dolist (name *editor-face-names*)
      (let* ((color-theme-args-for-face (getf color-theme-args name))
             (face-name (intern (string name) '#:editor))
             (face (editor:make-face face-name :if-exists t)))
        (apply 'editor:make-face face-name :if-exists :overwrite
                                           :documentation (or (getf color-theme-args-for-face :documentation)
                                                              (slot-value face 'documentation))
                                           color-theme-args-for-face))))

  theme-name)

(defun color-theme (theme-name)
  (mapc 'set-color-theme (color-theme-super-theme-names theme-name))
  (set-color-theme theme-name)

  (update-editor-panes)

  theme-name)

(defun define-color-theme (theme-name super-theme-names
                           &rest color-theme-args &key &allow-other-keys)
  (dolist (super-theme-name super-theme-names)
    (multiple-value-bind (color-theme-data found?)
        (gethash super-theme-name *all-color-themes*)
      (declare (ignore color-theme-data))
      (unless found?
        (warn "Inherited color theme ~s not defined." super-theme-name))))

  (setf (gethash theme-name *all-color-themes*) (list* super-theme-names color-theme-args))

  theme-name)

(defun remove-color-theme (theme-name)
  (remhash theme-name *all-color-themes*))

(sys::without-warning-on-redefinition
  (defmethod initialize-instance :around ((pane capi:editor-pane) &key &allow-other-keys)
    (multiple-value-prog1
        (call-next-method)

      (setf (gethash pane *all-editor-panes*) pane)

      (when *foreground-color*
        (setf (capi:simple-pane-foreground pane) *foreground-color*))
      (when *background-color*
        (setf (capi:simple-pane-background pane) *background-color*))))
  )

;; This makes it "work" after the podium is launched
(defun is-editor-pane-p (obj)
  (and (typep obj 'capi:editor-pane)
       (not (eq obj (hcl:class-prototype (class-of obj))))))

(defun cache-existing-pane (pane)
  (setf (gethash pane *all-editor-panes*) pane))

(defun cache-if-pane (obj)
  (when (is-editor-pane-p obj)
    (cache-existing-pane obj)))

#+:lispworks-personal-edition
(hcl:sweep-all-objects #'cache-if-pane)


;;; Initial color themes

(define-color-theme "default" ()
  :foreground nil :background nil
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face '(:background :green)
  :interactive-input-face '(:foreground :red3)
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face '(:foreground :blue)
  :font-lock-comment-face '(:foreground :firebrick)
  :font-lock-type-face '(:foreground :forestgreen)
  :font-lock-variable-name-face '(:foreground :darkgoldenrod)
  :font-lock-string-face '(:foreground :rosybrown)
  :font-lock-keyword-face '(:foreground :purple)
  :font-lock-builtin-face '(:foreground :orchid)
  :compiler-note-highlight '(:foreground :magenta)
  :compiler-warning-highlight '(:foreground :orange3)
  :compiler-error-highlight '(:foreground :red))

(define-color-theme "plain" ()
  :foreground nil :background nil
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face '()
  :interactive-input-face '()
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face '()
  :font-lock-comment-face '()
  :font-lock-type-face '()
  :font-lock-variable-name-face '()
  :font-lock-string-face '()
  :font-lock-keyword-face '()
  :font-lock-builtin-face '()
  :compiler-note-highlight '()
  :compiler-warning-highlight '()
  :compiler-error-highlight '())

(define-color-theme "emacs" ()
  :foreground nil :background nil
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face '(:background :green)
  :interactive-input-face '(:foreground :red3)
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face '(:foreground :blue)
  :font-lock-comment-face '(:foreground :gray40)
  :font-lock-type-face '(:foreground :forestgreen)
  :font-lock-variable-name-face '(:foreground :darkgoldenrod)
  :font-lock-string-face '(:foreground :rosybrown)
  :font-lock-keyword-face '(:foreground :purple)
  :font-lock-builtin-face '(:foreground :orchid)
  :compiler-note-highlight '(:foreground :magenta)
  :compiler-warning-highlight '(:foreground :orange3)
  :compiler-error-highlight '(:foreground :red))

(define-color-theme "torte" ()
  :foreground (color:make-rgb 0.8s0 0.8s0 0.8s0)
  :background (color:make-rgb 0.0s0 0.0s0 0.0s0)
  :region '(:foreground :color_highlighttext
            :background :color_highlight)
  :show-point-face `(:background ,(color:make-rgb 0.6275s0 0.1255s0 0.9412s0))
  :interactive-input-face '(:foreground :pink)
  :highlight '(:bold-p t)
  :non-focus-complete-face '(:background :tweak_background)
  :font-lock-function-name-face `(:foreground ,(color:make-rgb 0.0s0 1.0s0 1.0s0))
  :font-lock-comment-face `(:foreground ,(color:make-rgb 0.5s0 0.6275s0 1.0s0))
  :font-lock-type-face `(:foreground ,(color:make-rgb 0.5s0 1.0s0 0.5s0))
  :font-lock-variable-name-face `(:foreground ,(color:make-rgb 1.0s0 1.0s0 1.0s0))
  :font-lock-string-face `(:foreground ,(color:make-rgb 1.0s0 0.6275s0 0.6275s0))
  :font-lock-keyword-face `(:foreground ,(color:make-rgb 1.0s0 1.0s0 0.0s0))
  :font-lock-builtin-face `(:foreground ,(color:make-rgb 1.0s0 1.0s0 0.0s0))
  :compiler-note-highlight '(:foreground :magenta)
  :compiler-warning-highlight '(:foreground :orange)
  :compiler-error-highlight '(:foreground :red))


(defun make-rgb (red green blue &optional alpha)
  (color:make-rgb (/ red 255s0)
                  (/ green 255s0)
                  (/ blue 255s0)
                  (and alpha (/ alpha 255s0))))

(defvar *solarized-color-table*
  '(:solarized-base03  (#x00 #x2b #x36)
    :solarized-base02  (#x07 #x36 #x42)
    :solarized-base01  (#x58 #x6e #x75)
    :solarized-base00  (#x65 #x7b #x83)
    :solarized-base0   (#x83 #x94 #x96)
    :solarized-base1   (#x93 #xa1 #xa1)
    :solarized-base2   (#xee #xe8 #xd5)
    :solarized-base3   (#xfd #xf6 #xe3)
    :solarized-yellow  (#xb5 #x89 #x00)
    :solarized-orange  (#xcb #x4b #x16)
    :solarized-red     (#xdc #x32 #x2f)
    :solarized-magenta (#xd3 #x36 #x82)
    :solarized-violet  (#x6c #x71 #xc4)
    :solarized-blue    (#x26 #x8b #xd2)
    :solarized-cyan    (#x2a #xa1 #x98)
    :solarized-green   (#x85 #x99 #x00)))

(loop for list on *solarized-color-table* by #'cddr
      for name = (first list)
      for rgb = (second list)
      do
         (color:define-color-alias
             name
             (apply #'make-rgb rgb)))

(define-color-theme "solarized-light" ()
  :foreground :solarized-base00
  :background :solarized-base3
  :region '(:foreground :solarized-base1
            :background :solarized-base3
            :inverse-p t)
  :highlight '(:background :solarized-base2)
  :font-lock-function-name-face '(:foreground :solarized-blue)
  :font-lock-comment-face '(:foreground :solarized-base1 :italic-p t)
  :font-lock-type-face '(:foreground :solarized-yellow)
  :font-lock-variable-name-face '(:foreground :solarized-blue)
  :font-lock-string-face '(:foreground :solarized-cyan)
  :font-lock-keyword-face '(:foreground :solarized-green)
  :font-lock-builtin-face '(:foreground :solarized-green)
  :compiler-note-highlight '(:foreground :solarized-green
                             :bold-p t)
  :compiler-warning-highlight '(:foreground :solarized-orange
                                :bold-p t)
  :compiler-error-highlight '(:foreground :solarized-red
                              :inverse-p t)
  :show-point-face '(:foreground :solarized-cyan
                     :bold-p t :inverse-p t)
  :interactive-input-face '(:foreground :solarized-red)
  :non-focus-complete-face '(:background :solarized-base3)
  :parenthesis-font-face-colours '(:solarized-red
                                   :solarized-base01
                                   :solarized-green
                                   :solarized-orange
                                   :solarized-blue
                                   :solarized-magenta))

(define-color-theme "solarized-dark" ()
  :foreground :solarized-base0
  :background :solarized-base03
  :region '(:foreground :solarized-base01
            :background :solarized-base03
            :inverse-p t)
  :highlight '(:background :solarized-base02)
  :font-lock-function-name-face '(:foreground :solarized-blue)
  :font-lock-comment-face '(:foreground :solarized-base01 :italic-p t)
  :font-lock-type-face '(:foreground :solarized-yellow)
  :font-lock-variable-name-face '(:foreground :solarized-blue)
  :font-lock-string-face '(:foreground :solarized-cyan)
  :font-lock-keyword-face '(:foreground :solarized-green)
  :font-lock-builtin-face '(:foreground :solarized-green)
  :compiler-note-highlight '(:foreground :solarized-green
                             :bold-p t)
  :compiler-warning-highlight '(:foreground :solarized-orange
                                :bold-p t)
  :compiler-error-highlight '(:foreground :solarized-red
                              :inverse-p t)
  :show-point-face '(:foreground :solarized-cyan
                     :bold-p t :inverse-p t)
  :interactive-input-face '(:foreground :solarized-red)
  :non-focus-complete-face '(:background :solarized-base03)
  :parenthesis-font-face-colours '(:solarized-red
                                   :solarized-base1
                                   :solarized-green
                                   :solarized-orange
                                   :solarized-blue
                                   :solarized-magenta))


(defun hex->color (hex)
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (check-type hex (string 7))
  (flet ((extract-digits (string start end)
           (check-type string (simple-string 7))
           (parse-integer string
                          :start start
                          :end end
                          :radix 16)))
    (let* ((hex (coerce hex 'simple-string))
           (r (extract-digits hex 1 3))
           (g (extract-digits hex 3 5))
           (b (extract-digits hex 5 7)))
      (color:make-rgb (/ r 255.0)
                      (/ g 255.0)
                      (/ b 255.0)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +zenburn-colors+
    `((zenburn-fg+2 ,(hex->color "#FFFFEF"))
      (zenburn-fg+1 ,(hex->color "#F5F5D6"))
      (zenburn-fg ,(hex->color "#DCDCCC"))
      (zenburn-fg-1 ,(hex->color "#A6A689"))
      (zenburn-fg-2 ,(hex->color "#656555"))
      (zenburn-black ,(hex->color "#000000"))
      (zenburn-bg-2 ,(hex->color "#000000"))
      (zenburn-bg-1 ,(hex->color "#111112"))
      (zenburn-bg-05 ,(hex->color "#383838"))
      (zenburn-bg ,(hex->color "#2A2B2E"))
      (zenburn-bg+05 ,(hex->color "#494949"))
      (zenburn-bg+1 ,(hex->color "#4F4F4F"))
      (zenburn-bg+2 ,(hex->color "#5F5F5F"))
      (zenburn-bg+3 ,(hex->color "#6F6F6F"))
      (zenburn-red+2 ,(hex->color "#ECB3B3"))
      (zenburn-red+1 ,(hex->color "#DCA3A3"))
      (zenburn-red ,(hex->color "#CC9393"))
      (zenburn-red-1 ,(hex->color "#BC8383"))
      (zenburn-red-2 ,(hex->color "#AC7373"))
      (zenburn-red-3 ,(hex->color "#9C6363"))
      (zenburn-red-4 ,(hex->color "#8C5353"))
      (zenburn-red-5 ,(hex->color "#7C4343"))
      (zenburn-red-6 ,(hex->color "#6C3333"))
      (zenburn-orange ,(hex->color "#DFAF8F"))
      (zenburn-yellow ,(hex->color "#F0DFAF"))
      (zenburn-yellow-1 ,(hex->color "#E0CF9F"))
      (zenburn-yellow-2 ,(hex->color "#D0BF8F"))
      (zenburn-green-5 ,(hex->color "#2F4F2F"))
      (zenburn-green-4 ,(hex->color "#3F5F3F"))
      (zenburn-green-3 ,(hex->color "#4F6F4F"))
      (zenburn-green-2 ,(hex->color "#5F7F5F"))
      (zenburn-green-1 ,(hex->color "#6F8F6F"))
      (zenburn-green ,(hex->color "#7F9F7F"))
      (zenburn-green+1 ,(hex->color "#8FB28F"))
      (zenburn-green+2 ,(hex->color "#9FC59F"))
      (zenburn-green+3 ,(hex->color "#AFD8AF"))
      (zenburn-green+4 ,(hex->color "#BFEBBF"))
      (zenburn-cyan ,(hex->color "#93E0E3"))
      (zenburn-blue+3 ,(hex->color "#BDE0F3"))
      (zenburn-blue+2 ,(hex->color "#ACE0E3"))
      (zenburn-blue+1 ,(hex->color "#94BFF3"))
      (zenburn-blue ,(hex->color "#8CD0D3"))
      (zenburn-blue-1 ,(hex->color "#7CB8BB"))
      (zenburn-blue-2 ,(hex->color "#6CA0A3"))
      (zenburn-blue-3 ,(hex->color "#5C888B"))
      (zenburn-blue-4 ,(hex->color "#4C7073"))
      (zenburn-blue-5 ,(hex->color "#366060"))
      (zenburn-magenta ,(hex->color "#DC8CC3")))))

(defmacro with-zenburn-colors (&body body)
  `(let ,+zenburn-colors+
     (declare (ignorable ,@(mapcar 'car +zenburn-colors+)))
     ,@body))

(with-zenburn-colors
  (define-color-theme "zenburn" ()
    :foreground zenburn-fg
    :background zenburn-bg
    :region `(:foreground ,zenburn-fg+1
              :background ,zenburn-bg+1)
    :show-point-face `(:background ,zenburn-bg+2)
    :interactive-input-face `(:foreground ,zenburn-red)
    :highlight '(:bold-p t)
    :non-focus-complete-face `(:background :tweak_background)
    :font-lock-function-name-face `(:foreground ,zenburn-blue)
    :font-lock-comment-face `(:foreground ,zenburn-fg-1)
    :font-lock-type-face `(:foreground ,zenburn-green)
    :font-lock-variable-name-face `(:foreground ,zenburn-yellow)
    :font-lock-string-face `(:foreground ,zenburn-orange)
    :font-lock-keyword-face `(:foreground ,zenburn-cyan)
    :font-lock-builtin-face `(:foreground ,zenburn-blue+1)
    :compiler-note-highlight `(:foreground ,zenburn-fg+1)
    :compiler-warning-highlight `(:foreground ,zenburn-orange)
    :compiler-error-highlight `(:foreground ,zenburn-red+1)))

(defun zenburn-paren-colors ()
  (with-zenburn-colors
    (capi:set-editor-parenthesis-colors
     (list zenburn-red
           zenburn-green
           zenburn-blue-1
           zenburn-green+1
           zenburn-blue+1
           zenburn-green+2
           zenburn-orange
           zenburn-cyan
           zenburn-magenta
           zenburn-yellow))))

;;; Show presence when loaded
(pushnew :editor-color-theme *features*)
