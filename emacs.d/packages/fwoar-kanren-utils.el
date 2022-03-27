;;; fwoar-kanren-utils.el --- more functional utilities for emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Edward Langley

;; Author: Edward Langley <fwoar@elangley.org>
;; Version: 0.0.1
;; Keywords: minikanren,dom
;; URL: https://fwoar.co
;; Package-Requires: (reazon)

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

;; Some kanren utilities for dealing with the libxml dom

;;; Code:
(require 'reazon)

(reazon-defrel fwoar/tago (tag dom)
  (reazon-caro dom tag))

(reazon-defrel fwoar/attro (attr value dom)
  (reazon-fresh (attrs attr-pair dom-cdr)
    (reazon-cdro dom dom-cdr)
    (reazon-caro dom-cdr attrs)
    (reazon-membero attr-pair attrs)
    (reazon-conso attr value attr-pair)))

(reazon-defrel fwoar/childreno (children dom)
  (reazon-fresh (attrs attr-pair dom-cdr)
    (reazon-cdro dom dom-cdr)
    (reazon-cdro dom-cdr children)))

(reazon-defrel fwoar/childo (child dom)
  (reazon-fresh (children)
    (fwoar/childreno children dom)
    (reazon-membero child children)))

(reazon-defrel fwoar/dom-searcho (child dom)
  (reazon-disj
   (reazon-== child dom)
   (reazon-fresh (int)
     (reazon-conj
      (fwoar/childo int dom)
      (fwoar/dom-searcho child int)))))


(provide 'fwoar-kanren-utils)
;;; fwoar-kanren-utils.el ends here
