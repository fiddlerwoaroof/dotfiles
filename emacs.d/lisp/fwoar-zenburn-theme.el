(deftheme fwoar-zenburn
  "Created 2018-10-01.")

;; (defface centaur-tabs-selected
;;   '((t (:background "#31343E" :foreground "white")))
;;   "Face used for the selected tab."
;;   :group 'centaur-tabs)

(defgroup fwoar-zenburn ()
  "fwoar zenburn theme"
  :group 'faces)


(defvar fwoar-zenburn-default-colors-alist
  '(("zenburn-fg+2"     . "#FFFFEF")
    ("zenburn-fg+1"     . "#F5F5D6")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg-1"     . "#A6A689")
    ("zenburn-fg-2"     . "#656555")
    ("zenburn-black"    . "#000000")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#2B2B2B")
    ("zenburn-bg-05"    . "#383838")
    ("zenburn-bg"       . "#3F3F3F")
    ("zenburn-bg+05"    . "#494949")
    ("zenburn-bg+1"     . "#4F4F4F")
    ("zenburn-bg+2"     . "#5F5F5F")
    ("zenburn-bg+3"     . "#6F6F6F")
    ("zenburn-red+2"    . "#ECB3B3")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-red-5"    . "#7C4343")
    ("zenburn-red-6"    . "#6C3333")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-green-5"  . "#2F4F2F")
    ("zenburn-green-4"  . "#3F5F3F")
    ("zenburn-green-3"  . "#4F6F4F")
    ("zenburn-green-2"  . "#5F7F5F")
    ("zenburn-green-1"  . "#6F8F6F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+3"   . "#BDE0F3")
    ("zenburn-blue+2"   . "#ACE0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro fwoar/zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ,(mapcar (lambda (cons)
                   `(,(intern (car cons)) ,(cdr cons)))
                 fwoar-zenburn-default-colors-alist)
     ,@body))

(fwoar/zenburn-with-color-variables
  (defface fwoar-zenburn-1-face
    `((default :foreground ,zenburn-fg-1 :background ,zenburn-bg-1))
    "fwoar-zenburn-1" :group 'fwoar-zenburn)
  (defface fwoar-zenburn-face
    `((default :foreground ,zenburn-fg :background ,zenburn-bg))
    "fwoar-zenburn" :group 'fwoar-zenburn)
  (defface fwoar-zenburn+1-face
    `((default :foreground ,zenburn-fg+1 :background ,zenburn-bg+1))
    "fwoar-zenburn+1" :group 'fwoar-zenburn)
  (defface fwoar-zenburn+2-face
    `((default :foreground ,zenburn-fg+2 :background ,zenburn-bg+2))
    "fwoar-zenburn+2" :group 'fwoar-zenburn))


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


(fwoar/zenburn-with-color-variables
  (custom-theme-set-faces
   'fwoar-zenburn
   `(highlight ((t (:inverse-video t))))
   `(font-lock-builtin-face ((t (:weight bold))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-green))))
   `(font-lock-comment-face ((t (:foreground ,zenburn-green))))
   `(font-lock-constant-face ((t (:foreground ,zenburn-green+1))))
   `(font-lock-doc-face ((t (:foreground ,zenburn-yellow-2))))
   `(font-lock-function-name-face ((t (:foreground ,zenburn-blue))))
   `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenburn-cyan))))
   `(font-lock-string-face ((t (:foreground ,zenburn-yellow-2))))
   `(font-lock-type-face ((t (:foreground ,zenburn-red))))
   `(font-lock-variable-name-face ((t (:foreground ,zenburn-magenta))))
   `(font-lock-warning-face ((t (:foreground ,zenburn-red+2 :weight bold))))
   `(window-divider ((t (
                         :inherit fwoar-zenburn-1-face))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))
   `(header-line ((t (:weight light :box nil :inherit fwoar-zenburn-1-face))))
   `(centaur-tabs-selected ((t (:inherit fwoar-zenburn-face))))
   `(centaur-tabs-selected-modified ((t (:foreground ,zenburn-yellow-2 :inherit centaur-tabs-selected))))
   `(centaur-tabs-unselected ((t (:foreground unspecified :inherit header-line))))
   `(centaur-tabs-unselected-modified ((t (:foreground ,zenburn-yellow-2 :inherit centaur-tabs-unselected))))
   `(centaur-tabs-close-unselected ((t (:foreground unspecified :inherit header-line))))
   `(fringe ((t (:inherit fwoar-zenburn-face))))
   `(line-number ((t (:foreground unspecified :background unspecified :inherit fwoar-zenburn-face))))
   `(line-number-current-line ((t (:foreground ,zenburn-blue+1 :inherit fwoar-zenburn-1-face))))
   `(mode-line-highlight ((t nil)))
   `(mode-line ((t (:background unspecified :foreground unspecified :box nil :overline ,zenburn-fg
                                :inherit fwoar-zenburn-1-face))))
   `(mode-line-inactive ((t (:background unspecified :foreground unspecified :box nil :overline ,zenburn-fg-1 
                                         :inherit fwoar-zenburn-1-face))))
   `(mode-line-buffer-id ((t (:foreground ,zenburn-blue-2))))
   `(region ((t (:foreground unspecified :background unspecified :inverse-video t))))
   `(tool-bar ((t (:background unspecified :foreground unspecified :box nil :inherit fwoar-zenburn+2-face))))
   `(variable-pitch ((t (:family "Lato"))))
   `(vertical-border ((t (:foreground "slate gray"))))
   `(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-face :background ,zenburn-red))))
   `(treemacs-root-face ((t (:inherit font-lock-constant-face :height 1.5))))
   `(minibuffer-prompt ((t :foreground ,zenburn-fg)))
   `(cursor ((t :foreground unspecified :background unspecified :inverse-video t)))
   `(default ((t (:family "Source Code Pro" :foundry "nil"
                          :slant normal :weight normal :height 130 :width normal
                          :foreground ,zenburn-fg :background ,zenburn-bg 
                          :inherit fwoar-zenburn-face))))
   ;; helm
   `(helm-source-header ((t :inherit fwoar-zenburn+1-face)))
   `(helm-M-x-key ((t (:foreground ,zenburn-orange))))
   `(helm-candidate-number ((t (:foreground ,zenburn-yellow))))
   `(helm-selection ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg-1))))
   `(helm-match ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-2 :box ,zenburn-yellow-1))))

   `(helm-ff-denied ((t (:foreground ,zenburn-red :background ,zenburn-black))))
   `(helm-ff-directory ((t (:foreground ,zenburn-cyan))))
   `(helm-ff-dotted-directory ((t (:foreground ,zenburn-cyan :background ,zenburn-bg+1))))
   `(helm-ff-dotted-symlink-directory ((t (:foreground ,zenburn-orange :background ,zenburn-bg+1))))
   `(helm-ff-executable ((t (:foreground ,zenburn-green))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenburn-black :background ,zenburn-red))))
   `(helm-ff-pipe ((t (:foreground ,zenburn-yellow :background ,zenburn-black))))
   `(helm-ff-prefix ((t (:foreground ,zenburn-black :background ,zenburn-yellow))))
   `(helm-ff-socket ((t (:foreground ,zenburn-red+2 :background unspecified))))
   `(helm-ff-suid ((t (:foreground ,zenburn-fg+1 :background ,zenburn-red))))
   `(helm-grep-file ((t (:foreground "BlueViolet" :background unspecified))))
   `(helm-grep-finish ((t (:foreground "Green" :background unspecified))))
   `(helm-grep-lineno ((t (:foreground "Darkorange1" :background unspecified))))
   `(helm-grep-match ((t (:foreground "gold1" :background unspecified))))
   `(helm-header-line-left-margin ((t (:foreground ,zenburn-black :background ,zenburn-yellow))))
   `(helm-history-remote ((t (:foreground "Indianred1" :background unspecified))))
   `(helm-lisp-completion-info ((t (:foreground ,zenburn-red :background unspecified))))
   `(helm-lisp-show-completion ((t (:foreground unspecified :background "DarkSlateGray"))))
   `(helm-locate-finish ((t (:foreground "Green" :background unspecified))))
   `(helm-ls-git-added-copied-face ((t (:foreground ,zenburn-green :background unspecified))))
   `(helm-ls-git-added-modified-face ((t (:foreground ,zenburn-blue :background unspecified))))
   `(helm-ls-git-conflict-face ((t (:foreground "MediumVioletRed" :background unspecified))))
   `(helm-ls-git-deleted-and-staged-face ((t (:foreground "DimGray" :background unspecified))))
   `(helm-ls-git-deleted-not-staged-face ((t (:foreground "Darkgoldenrod3" :background unspecified))))
   `(helm-ls-git-modified-and-staged-face ((t (:foreground "Goldenrod" :background unspecified))))
   `(helm-ls-git-modified-not-staged-face ((t (:foreground ,zenburn-yellow :background unspecified))))
   `(helm-ls-git-renamed-modified-face ((t (:foreground "Goldenrod" :background unspecified))))
   `(helm-ls-git-untracked-face ((t (:foreground ,zenburn-red :background unspecified))))
   `(helm-moccur-buffer ((t (:foreground "DarkTurquoise" :background unspecified))))
   `(helm-prefarg ((t (:foreground ,zenburn-green :background unspecified))))
   `(helm-resume-need-update ((t (:foreground unspecified :background ,zenburn-red))))
   `(helm-rg-active-arg-face ((t (:foreground ,zenburn-green :background unspecified))))
   `(helm-rg-base-rg-cmd-face ((t (:foreground "gray" :background unspecified))))
   `(helm-rg-colon-separator-ripgrep-output-face ((t (:foreground "white" :background unspecified))))
   `(helm-rg-directory-cmd-face ((t (:foreground "brown" :background ,zenburn-black))))
   `(helm-rg-directory-header-face ((t (:foreground "white" :background ,zenburn-black))))
   `(helm-rg-error-message ((t (:foreground ,zenburn-red :background unspecified))))
   `(helm-rg-extra-arg-face ((t (:foreground ,zenburn-yellow :background unspecified))))
   `(helm-rg-file-match-face ((t (:foreground "#0ff" :background unspecified))))
   `(helm-rg-inactive-arg-face ((t (:foreground "gray" :background unspecified))))
   `(helm-rg-line-number-match-face ((t (:foreground ,zenburn-orange :background unspecified))))
   `(helm-rg-match-text-face ((t (:foreground "white" :background "purple"))))
   `(helm-rg-preview-line-highlight ((t (:foreground ,zenburn-black :background ,zenburn-green))))
   `(helm-rg-title-face ((t (:foreground "purple" :background ,zenburn-black))))
   `(helm-selection ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,zenburn-red :background unspecified))))
   `(helm-visible-mark ((t (:foreground unspecified :background unspecified))))
   ;;company
   `(company-echo-common ((t (:foreground ,zenburn-red-1 :background unspecified))))
   `(company-preview ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg-2))))
   `(company-preview-common ((t (:inherit company-preview))))
   `(company-preview-search ((t (:foreground ,zenburn-blue+2 :inherit company-preview))))
   `(company-scrollbar-bg ((t (:foreground unspecified :background ,zenburn-bg-2))))
   `(company-scrollbar-fg ((t (:foreground unspecified :background ,zenburn-fg-1))))
   `(company-template-field ((t (:foreground ,zenburn-black :background ,zenburn-yellow))))
   `(company-tooltip ((t (:inherit fwoar-zenburn+1-face))))
   `(company-tooltip-annotation ((t (:foreground ,zenburn-red :background unspecified))))
   `(company-tooltip-common ((t (:foreground ,zenburn-red :background unspecified))))
   `(company-tooltip-selection ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg-1))))

   ;;slime
   `(slime-repl-inputed-output-face ((t (:foreground ,zenburn-blue))))

   ;; javascript
   `(js2-function-param ((t (:foreground ,zenburn-green))))
   `(js2-external-variable ((t (:foreground ,zenburn-orange))))
   `(js2-error ((t (:foreground ,zenburn-red+2))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:style wave :color ,zenburn-red-2)))))

   ;;magit
   `(magit-diff-added ((t (:foreground ,zenburn-fg-1 :background ,zenburn-green-5))))
   `(magit-diff-removed ((t (:foreground ,zenburn-fg-1 :background ,zenburn-red-5))))
   `(magit-diff-added-highlight ((t (:foreground ,zenburn-fg+1 :background ,zenburn-green-3))))
   `(magit-diff-removed-highlight ((t (:foreground ,zenburn-fg+1 :background ,zenburn-red-3))))

   `(magit-section ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))
   `(magit-section-highlight ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg+1))))

   `(magit-diff-base ((t (:foreground unspecified :background unspecified))))
   `(magit-diff-base-highlight ((t (:foreground unspecified :background unspecified))))

   `(magit-diff-context ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg-2))))
   `(magit-diff-context-highlight ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))

   `(magit-diff-file-heading ((t (:background ,zenburn-bg+05))))
   `(magit-diff-file-heading-highlight ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg+1))))
   `(magit-diff-hunk-heading ((t (:background ,zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg+2))))

   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'fwoar-zenburn)
