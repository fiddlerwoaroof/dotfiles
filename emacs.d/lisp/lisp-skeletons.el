;;; lisp-skeletons.el --- Skeletons for generating Common Lisp code       -*- lexical-binding: t; tab-width: 8; -*-

;; Copyright (C) 2017 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:

;; code goes here
(require 'skeleton)
(require 'evil)

(defvar *skeleton-markers* nil
  "Markers for locations saved in skeleton-positions.")

(defun skeleton-make-markers ()
  "..."
  (while *skeleton-markers*
    (set-marker (pop *skeleton-markers*) nil))
  (setq *skeleton-markers*
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defun skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton.
REVERSE - Jump to previous position in skeleton"
  (interactive "P")

  (let* ((positions (mapcar 'marker-position *skeleton-markers*))
         (positions (if reverse (reverse positions) positions))
         (comp (if reverse '> '<))
         pos)
    (when positions
      (if (catch 'break
            (while (setq pos (pop positions))
              (when (funcall comp (point) pos)
                (throw 'break t))))
          (goto-char pos)
        (goto-char (marker-position
                    (car *skeleton-markers*))))))
  (when (fboundp 'evil-insert)
    (evil-insert 1)))

(defun skeleton-prev-position ()
      (interactive "P")
      (skeleton-next-position t))

(define-skeleton skel-defun
  "Insert a defun template."
  "Name: "
  "(defun " str " (" @ - ")" \n
  @ _ ")"  \n
  '(when (fboundp 'evil-insert)
     (evil-insert 1)))

(define-skeleton skel-defmacro
  "Insert a defmacro template."
  "Name: "
  "(defmacro " str " (" @ - ")" \n
  @ _ ")"  \n
  '(evil-insert 1))

(define-skeleton skel-defparameter
  "Insert a defmacro template."
  "Name: "
  "(defparameter " str @ _ ")"  \n
  '(evil-insert 1))

(define-skeleton skel-defvar
  "Insert a defmacro template."
  "Name: "
  "(defvar " str @ _ ")"  \n
  '(evil-insert 1))

(defvar fwoar::*package-prefix* "fwoar"
  "A prefix applied to lisp packages")

(define-skeleton skel-defpackage
  "Insert a defpackage template"
  (skeleton-read "Package Name: "
                 (if v1
                     (format "%s.%s"
                             fwoar::*package-prefix*
                             (file-name-sans-extension
                              (file-name-nondirectory
                               (buffer-file-name))))))
  '(setq v1 (bobp))
  "(defpackage :" @ str "
  (:use :cl "  _ @ - ")
  (:export " @  "))
(in-package :" str ")" \n
  @)


(define-skeleton skel-defsystem
  "Insert a defsystem template"
  (skeleton-read "System Name: " (if v1
                                     (file-name-sans-extension
                                      (file-name-nondirectory
                                       (buffer-file-name)))))
  & (if (setq v1 (bobp))
        ";;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-")
  & \n
  & "(in-package :asdf-user)"
  & \n
  & \n
  "(defsystem :" @ str " 
  :description \"\"
  :author \"Ed L <edward@elangley.org>\"
  :license \"MIT\"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               " @ - ")
  :serial t
  :components (" @ "
  " _ "))"
  '(save-excursion
     (indent-region (point-min) (point-max) nil)))
(provide 'lisp-skeletons)
