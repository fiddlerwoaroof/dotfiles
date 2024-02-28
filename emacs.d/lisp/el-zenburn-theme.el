(deftheme fwoar-zenburn
  "Created 2018-10-01.")

(defvar fwoar-zenburn-default-colors-alist

  "List of Zenburn colors.
Each element has the form (NAME . HEX).
`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

;; (defface centaur-tabs-selected
;;   '((t (:background "#31343E" :foreground "white")))
;;   "Face used for the selected tab."
;;   :group 'centaur-tabs)

(defgroup fwoar-zenburn ()
  "fwoar zenburn theme"
  :group 'faces)

(defmacro fwoar:facify-alist (&rest specs)
  `(progn ,@(mapcan (lambda (spec)
                      (cl-destructuring-bind (name . value) spec
                        `((defface ,(make-symbol (format "%s-face" name))
                            '((default ,@value))
                            ,(format "%s" name)
                            :group 'fwoar-zenburn)
                          ,@(cl-loop
                             for (type val) on (cdr spec) by 'cddr
                             collect `(defvar ,(make-symbol (format "+%s-%s+"
                                                                    name type))
                                        ,val)))))
                    specs)))

(progn
  (defface fwoar-zenburn-1-face
    '((default :foreground "#656555" :background "#2B2B2B"))
    "fwoar-zenburn-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-1-:foreground+ "#656555")
  (defvar +fwoar-zenburn-1-:background+ "#2B2B2B")
  (defface fwoar-zenburn-05-face
    '((default :foreground "#989890" :background "#383838"))
    "fwoar-zenburn-05" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-05-:foreground+ "#989890")
  (defvar +fwoar-zenburn-05-:background+ "#383838")
  (defface fwoar-zenburn-face
    '((default :foreground "#DCDCCC" :background "#3f3f3f"))
    "fwoar-zenburn" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-:foreground+ "#DCDCCC")
  (defvar +fwoar-zenburn-:background+ "#3F3F3F")
  (defface fwoar-zenburn+1-face
    '((default :foreground "#FFFFEF" :background "#4F4F4F"))
    "fwoar-zenburn+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn+1-:foreground+ "#FFFFEF")
  (defvar +fwoar-zenburn+1-:background+ "#4F4F4F")
  (defface fwoar-zenburn+2-face
    '((default :foreground "#FFFFFD" :background "#5F5F5F"))
    "fwoar-zenburn+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn+2-:foreground+ "#FFFFFD")
  (defvar +fwoar-zenburn+2-:background+ "#5F5F5F")
  (defface fwoar-zenburn-bg+1-face
    '((default :background "#4F4F4F"))
    "fwoar-zenburn-bg+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg+1-:background+ "#4F4F4F")
  (defface fwoar-zenburn-bg+2-face
    '((default :background "#5F5F5F"))
    "fwoar-zenburn-bg+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg+2-:background+ "#5F5F5F")
  (defface fwoar-zenburn-bg-05-face
    '((default :background "#383838"))
    "fwoar-zenburn-bg-05" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-05-:background+ "#383838")
  (defface fwoar-zenburn-bg-1-face
    '((default :background "#2B2B2B"))
    "fwoar-zenburn-bg-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-1-:background+ "#2B2B2B")
  (defface fwoar-zenburn-bg-face
    '((default :background "#3F3F3F"))
    "fwoar-zenburn-bg" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-:background+ "#3F3F3F")
  (defface fwoar-zenburn-fg+1-face
    '((default :foreground "#FFFFEF"))
    "fwoar-zenburn-fg+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg+1-:foreground+ "#FFFFEF")
  (defface fwoar-zenburn-fg+2-face
    '((default :foreground "#FFFFFD"))
    "fwoar-zenburn-fg+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg+2-:foreground+ "#FFFFFD")
  (defface fwoar-zenburn-fg-05-face
    '((default :foreground "#989890"))
    "fwoar-zenburn-fg-05" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-05-:foreground+ "#989890")
  (defface fwoar-zenburn-fg-1-face
    '((default :foreground "#656555"))
    "fwoar-zenburn-fg-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-1-:foreground+ "#656555")
  (defface fwoar-zenburn-fg-face
    '((default :foreground "#DCDCCC"))
    "fwoar-zenburn-fg" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-:foreground+ "#DCDCCC")
  (defface fwoar-zenburn-bg-2-face
    '((default :background "#000000"))
    "fwoar-zenburn-bg-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-2-:background+ "#000000")
  (defface fwoar-zenburn-bg-08-face
    '((default :background "#303030"))
    "fwoar-zenburn-bg-08" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-08-:background+ "#303030")
  (defface fwoar-zenburn-bg+05-face
    '((default :background "#494949"))
    "fwoar-zenburn-bg+05" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg+05-:background+ "#494949")
  (defface fwoar-zenburn-bg+3-face
    '((default :background "#6F6F6F"))
    "fwoar-zenburn-bg+3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg+3-:background+ "#6F6F6F")
  (defface fwoar-zenburn-bg-blue+1-face
    '((default :background "#94BFF3"))
    "fwoar-zenburn-bg-blue+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue+1-:background+ "#94BFF3")
  (defface fwoar-zenburn-bg-blue+2-face
    '((default :background "#ACE0E3"))
    "fwoar-zenburn-bg-blue+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue+2-:background+ "#ACE0E3")
  (defface fwoar-zenburn-bg-blue+3-face
    '((default :background "#BDE0F3"))
    "fwoar-zenburn-bg-blue+3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue+3-:background+ "#BDE0F3")
  (defface fwoar-zenburn-bg-blue-1-face
    '((default :background "#7CB8BB"))
    "fwoar-zenburn-bg-blue-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue-1-:background+ "#7CB8BB")
  (defface fwoar-zenburn-bg-blue-2-face
    '((default :background "#6CA0A3"))
    "fwoar-zenburn-bg-blue-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue-2-:background+ "#6CA0A3")
  (defface fwoar-zenburn-bg-blue-3-face
    '((default :background "#5C888B"))
    "fwoar-zenburn-bg-blue-3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue-3-:background+ "#5C888B")
  (defface fwoar-zenburn-bg-blue-4-face
    '((default :background "#4C7073"))
    "fwoar-zenburn-bg-blue-4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue-4-:background+ "#4C7073")
  (defface fwoar-zenburn-bg-blue-5-face
    '((default :background "#366060"))
    "fwoar-zenburn-bg-blue-5" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue-5-:background+ "#366060")
  (defface fwoar-zenburn-bg-blue-face
    '((default :background "#8CD0D3"))
    "fwoar-zenburn-bg-blue" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-blue-:background+ "#8CD0D3")
  (defface fwoar-zenburn-bg-cyan-face
    '((default :background "#93E0E3"))
    "fwoar-zenburn-bg-cyan" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-cyan-:background+ "#93E0E3")
  (defface fwoar-zenburn-bg-green+1-face
    '((default :background "#8FB28F"))
    "fwoar-zenburn-bg-green+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green+1-:background+ "#8FB28F")
  (defface fwoar-zenburn-bg-green+2-face
    '((default :background "#9FC59F"))
    "fwoar-zenburn-bg-green+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green+2-:background+ "#9FC59F")
  (defface fwoar-zenburn-bg-green+3-face
    '((default :background "#AFD8AF"))
    "fwoar-zenburn-bg-green+3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green+3-:background+ "#AFD8AF")
  (defface fwoar-zenburn-bg-green+4-face
    '((default :background "#BFEBBF"))
    "fwoar-zenburn-bg-green+4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green+4-:background+ "#BFEBBF")
  (defface fwoar-zenburn-bg-green-1-face
    '((default :background "#6F8F6F"))
    "fwoar-zenburn-bg-green-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green-1-:background+ "#6F8F6F")
  (defface fwoar-zenburn-bg-green-2-face
    '((default :background "#5F7F5F"))
    "fwoar-zenburn-bg-green-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green-2-:background+ "#5F7F5F")
  (defface fwoar-zenburn-bg-green-3-face
    '((default :background "#4F6F4F"))
    "fwoar-zenburn-bg-green-3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green-3-:background+ "#4F6F4F")
  (defface fwoar-zenburn-bg-green-4-face
    '((default :background "#3F5F3F"))
    "fwoar-zenburn-bg-green-4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green-4-:background+ "#3F5F3F")
  (defface fwoar-zenburn-bg-green-5-face
    '((default :background "#2F4F2F"))
    "fwoar-zenburn-bg-green-5" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green-5-:background+ "#2F4F2F")
  (defface fwoar-zenburn-bg-green-face
    '((default :background "#7F9F7F"))
    "fwoar-zenburn-bg-green" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-green-:background+ "#7F9F7F")
  (defface fwoar-zenburn-bg-magenta-face
    '((default :background "#DC8CC3"))
    "fwoar-zenburn-bg-magenta" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-magenta-:background+ "#DC8CC3")
  (defface fwoar-zenburn-bg-orange-face
    '((default :background "#DFAF8F"))
    "fwoar-zenburn-bg-orange" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-orange-:background+ "#DFAF8F")
  (defface fwoar-zenburn-bg-red+1-face
    '((default :background "#DCA3A3"))
    "fwoar-zenburn-bg-red+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red+1-:background+ "#DCA3A3")
  (defface fwoar-zenburn-bg-red+2-face
    '((default :background "#ECB3B3"))
    "fwoar-zenburn-bg-red+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red+2-:background+ "#ECB3B3")
  (defface fwoar-zenburn-bg-red-1-face
    '((default :background "#BC8383"))
    "fwoar-zenburn-bg-red-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red-1-:background+ "#BC8383")
  (defface fwoar-zenburn-bg-red-2-face
    '((default :background "#AC7373"))
    "fwoar-zenburn-bg-red-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red-2-:background+ "#AC7373")
  (defface fwoar-zenburn-bg-red-3-face
    '((default :background "#9C6363"))
    "fwoar-zenburn-bg-red-3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red-3-:background+ "#9C6363")
  (defface fwoar-zenburn-bg-red-4-face
    '((default :background "#8C5353"))
    "fwoar-zenburn-bg-red-4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red-4-:background+ "#8C5353")
  (defface fwoar-zenburn-bg-red-5-face
    '((default :background "#7C4343"))
    "fwoar-zenburn-bg-red-5" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red-5-:background+ "#7C4343")
  (defface fwoar-zenburn-bg-red-6-face
    '((default :background "#6C3333"))
    "fwoar-zenburn-bg-red-6" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red-6-:background+ "#6C3333")
  (defface fwoar-zenburn-bg-red-face
    '((default :background "#CC9393"))
    "fwoar-zenburn-bg-red" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-red-:background+ "#CC9393")
  (defface fwoar-zenburn-bg-yellow-1-face
    '((default :background "#E0CF9F"))
    "fwoar-zenburn-bg-yellow-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-yellow-1-:background+ "#E0CF9F")
  (defface fwoar-zenburn-bg-yellow-2-face
    '((default :background "#D0BF8F"))
    "fwoar-zenburn-bg-yellow-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-yellow-2-:background+ "#D0BF8F")
  (defface fwoar-zenburn-bg-yellow-face
    '((default :background "#F0DFAF"))
    "fwoar-zenburn-bg-yellow" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-bg-yellow-:background+ "#F0DFAF")
  (defface fwoar-zenburn-fg-blue+1-face
    '((default :foreground "#94BFF3"))
    "fwoar-zenburn-fg-blue+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue+1-:foreground+ "#94BFF3")
  (defface fwoar-zenburn-fg-blue+2-face
    '((default :foreground "#ACE0E3"))
    "fwoar-zenburn-fg-blue+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue+2-:foreground+ "#ACE0E3")
  (defface fwoar-zenburn-fg-blue+3-face
    '((default :foreground "#BDE0F3"))
    "fwoar-zenburn-fg-blue+3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue+3-:foreground+ "#BDE0F3")
  (defface fwoar-zenburn-fg-blue-1-face
    '((default :foreground "#7CB8BB"))
    "fwoar-zenburn-fg-blue-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue-1-:foreground+ "#7CB8BB")
  (defface fwoar-zenburn-fg-blue-2-face
    '((default :foreground "#6CA0A3"))
    "fwoar-zenburn-fg-blue-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue-2-:foreground+ "#6CA0A3")
  (defface fwoar-zenburn-fg-blue-3-face
    '((default :foreground "#5C888B"))
    "fwoar-zenburn-fg-blue-3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue-3-:foreground+ "#5C888B")
  (defface fwoar-zenburn-fg-blue-4-face
    '((default :foreground "#4C7073"))
    "fwoar-zenburn-fg-blue-4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue-4-:foreground+ "#4C7073")
  (defface fwoar-zenburn-fg-blue-5-face
    '((default :foreground "#366060"))
    "fwoar-zenburn-fg-blue-5" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue-5-:foreground+ "#366060")
  (defface fwoar-zenburn-fg-blue-face
    '((default :foreground "#8CD0D3"))
    "fwoar-zenburn-fg-blue" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-blue-:foreground+ "#8CD0D3")
  (defface fwoar-zenburn-fg-cyan-face
    '((default :foreground "#93E0E3"))
    "fwoar-zenburn-fg-cyan" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-cyan-:foreground+ "#93E0E3")
  (defface fwoar-zenburn-fg-green+1-face
    '((default :foreground "#8FB28F"))
    "fwoar-zenburn-fg-green+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green+1-:foreground+ "#8FB28F")
  (defface fwoar-zenburn-fg-green+2-face
    '((default :foreground "#9FC59F"))
    "fwoar-zenburn-fg-green+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green+2-:foreground+ "#9FC59F")
  (defface fwoar-zenburn-fg-green+3-face
    '((default :foreground "#AFD8AF"))
    "fwoar-zenburn-fg-green+3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green+3-:foreground+ "#AFD8AF")
  (defface fwoar-zenburn-fg-green+4-face
    '((default :foreground "#BFEBBF"))
    "fwoar-zenburn-fg-green+4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green+4-:foreground+ "#BFEBBF")
  (defface fwoar-zenburn-fg-green-1-face
    '((default :foreground "#6F8F6F"))
    "fwoar-zenburn-fg-green-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green-1-:foreground+ "#6F8F6F")
  (defface fwoar-zenburn-fg-green-2-face
    '((default :foreground "#5F7F5F"))
    "fwoar-zenburn-fg-green-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green-2-:foreground+ "#5F7F5F")
  (defface fwoar-zenburn-fg-green-3-face
    '((default :foreground "#4F6F4F"))
    "fwoar-zenburn-fg-green-3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green-3-:foreground+ "#4F6F4F")
  (defface fwoar-zenburn-fg-green-4-face
    '((default :foreground "#3F5F3F"))
    "fwoar-zenburn-fg-green-4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green-4-:foreground+ "#3F5F3F")
  (defface fwoar-zenburn-fg-green-5-face
    '((default :foreground "#2F4F2F"))
    "fwoar-zenburn-fg-green-5" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green-5-:foreground+ "#2F4F2F")
  (defface fwoar-zenburn-fg-green-face
    '((default :foreground "#7F9F7F"))
    "fwoar-zenburn-fg-green" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-green-:foreground+ "#7F9F7F")
  (defface fwoar-zenburn-fg-magenta-face
    '((default :foreground "#DC8CC3"))
    "fwoar-zenburn-fg-magenta" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-magenta-:foreground+ "#DC8CC3")
  (defface fwoar-zenburn-fg-orange-face
    '((default :foreground "#DFAF8F"))
    "fwoar-zenburn-fg-orange" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-orange-:foreground+ "#DFAF8F")
  (defface fwoar-zenburn-fg-red+1-face
    '((default :foreground "#DCA3A3"))
    "fwoar-zenburn-fg-red+1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red+1-:foreground+ "#DCA3A3")
  (defface fwoar-zenburn-fg-red+2-face
    '((default :foreground "#ECB3B3"))
    "fwoar-zenburn-fg-red+2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red+2-:foreground+ "#ECB3B3")
  (defface fwoar-zenburn-fg-red-1-face
    '((default :foreground "#BC8383"))
    "fwoar-zenburn-fg-red-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red-1-:foreground+ "#BC8383")
  (defface fwoar-zenburn-fg-red-2-face
    '((default :foreground "#AC7373"))
    "fwoar-zenburn-fg-red-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red-2-:foreground+ "#AC7373")
  (defface fwoar-zenburn-fg-red-3-face
    '((default :foreground "#9C6363"))
    "fwoar-zenburn-fg-red-3" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red-3-:foreground+ "#9C6363")
  (defface fwoar-zenburn-fg-red-4-face
    '((default :foreground "#8C5353"))
    "fwoar-zenburn-fg-red-4" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red-4-:foreground+ "#8C5353")
  (defface fwoar-zenburn-fg-red-5-face
    '((default :foreground "#7C4343"))
    "fwoar-zenburn-fg-red-5" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red-5-:foreground+ "#7C4343")
  (defface fwoar-zenburn-fg-red-6-face
    '((default :foreground "#6C3333"))
    "fwoar-zenburn-fg-red-6" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red-6-:foreground+ "#6C3333")
  (defface fwoar-zenburn-fg-red-face
    '((default :foreground "#CC9393"))
    "fwoar-zenburn-fg-red" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-red-:foreground+ "#CC9393")
  (defface fwoar-zenburn-fg-yellow-1-face
    '((default :foreground "#E0CF9F"))
    "fwoar-zenburn-fg-yellow-1" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-yellow-1-:foreground+ "#E0CF9F")
  (defface fwoar-zenburn-fg-yellow-2-face
    '((default :foreground "#D0BF8F"))
    "fwoar-zenburn-fg-yellow-2" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-yellow-2-:foreground+ "#D0BF8F")
  (defface fwoar-zenburn-fg-yellow-face
    '((default :foreground "#F0DFAF"))
    "fwoar-zenburn-fg-yellow" :group 'fwoar-zenburn)
  (defvar +fwoar-zenburn-fg-yellow-:foreground+ "#F0DFAF"))

(custom-theme-set-variables
 'fwoar-zenburn
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(blink-matching-paren-distance 1024)
 '(centaur-tabs-close-button " × ")
 '(hl-paren-background-colors nil)
 '(hl-paren-colors (quote ("yellow1" "gold1" "gold2" "gold3" "gold4")))
 '(initial-frame-alist nil)
 '(mac-command-modifier (quote super))
 '(mac-option-modifier (quote meta))
 '(nrepl-message-colors (quote ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))))

(custom-theme-set-faces
 'fwoar-zenburn
 '(font-lock-builtin-face ((t (:weight bold))))
 '(font-lock-comment-delimiter-face ((t (:foreground unspecified :inherit fwoar-zenburn-fg-green-face))))
 '(font-lock-comment-face           ((t (:foreground unspecified :inherit fwoar-zenburn-fg-green-face))))
 '(font-lock-constant-face          ((t (:foreground unspecified :inherit fwoar-zenburn-fg-green+1-face))))
 '(font-lock-doc-face               ((t (:foreground unspecified :inherit fwoar-zenburn-fg-green+2-face))))
 '(font-lock-function-name-face     ((t (:foreground unspecified :inherit fwoar-zenburn-fg-blue-face))))
 '(font-lock-keyword-face           ((t (:foreground unspecified :inherit fwoar-zenburn-fg-yellow-face :weight bold))))
 '(font-lock-preprocessor-face      ((t (:foreground unspecified :inherit fwoar-zenburn-fg-cyan-face))))
 '(font-lock-string-face            ((t (:foreground unspecified :inherit fwoar-zenburn-fg-orange-face))))
 '(font-lock-type-face              ((t (:foreground unspecified :inherit fwoar-zenburn-fg-red-face))))
 '(font-lock-variable-name-face     ((t (:foreground unspecified :inherit fwoar-zenburn-fg-magenta-face))))
 '(font-lock-warning-face           ((t (:foreground unspecified :inherit fwoar-zenburn-fg-red+2-face :weight bold))))

 `(centaur-tabs-selected ((t (:foreground unspecified
                                          :inherit fwoar-zenburn-face))))
 `(centaur-tabs-selected-modified ((t (:foreground ,+fwoar-zenburn-fg-yellow-2-:foreground+
                                                   :inherit centaur-tabs-selected))))

 `(centaur-tabs-unselected ((t (:foreground unspecified :inherit header-line))))
 `(centaur-tabs-unselected-modified ((t (:foreground ,+fwoar-zenburn-fg-yellow-2-:foreground+
                                                     :inherit centaur-tabs-unselected))))
 `(centaur-tabs-close-unselected ((t (:foreground unspecified :inherit header-line))))

 '(fringe ((t (:inherit fwoar-zenburn-face))))
 '(line-number ((t (:foreground unspecified :background unspecified
                                :inherit fwoar-zenburn-face))))
 `(line-number-current-line ((t (:foreground ,+fwoar-zenburn-fg-blue+1-:foreground+
                                             :inherit fwoar-zenburn-face))))
 '(header-line ((t (:background unspecified :foreground unspecified :weight light :box nil
                                :inherit fwoar-zenburn-bg-1-face))))
 '(mode-line-highlight ((t nil)))
 `(mode-line ((t (:background unspecified :foreground unspecified :box nil
                              :overline ,+fwoar-zenburn-:foreground+
                              :inherit fwoar-zenburn-face))))
 `(mode-line-inactive ((t (:background unspecified :foreground unspecified :box nil
                                       :overline ,+fwoar-zenburn-1-:foreground+
                                       :inherit fwoar-zenburn-1-face))))
 `(mode-line-buffer-id ((t (:foreground ,+fwoar-zenburn-bg-blue-:background+))))

 '(region ((t (:foreground unspecified :background unspecified :inverse-video t))))
 '(tool-bar ((t (:background unspecified :foreground unspecified :box nil
                             :inherit fwoar-zenburn+2-face))))
 '(variable-pitch ((t (:family "Lato"))))
 '(vertical-border ((t (:foreground "slate gray"))))
 `(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-face
                                                   :background ,+fwoar-zenburn-bg-red-:background+))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :height 1.5))))
 '(default ((t (:family "Source Code Pro" :foundry "nil" :slant normal :weight normal
                        :height 130 :width normal
                        :inherit fwoar-zenburn-face)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'fwoar-zenburn)
