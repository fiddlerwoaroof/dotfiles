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
           #:remove-color-theme))

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


;;; Show presence when loaded
(pushnew :editor-color-theme *features*)
