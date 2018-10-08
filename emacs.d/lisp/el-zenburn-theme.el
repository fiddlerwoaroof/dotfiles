(deftheme el-zenburn
  "Created 2018-10-01.")

(custom-theme-set-variables
 'el-zenburn
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(blink-matching-paren-distance 1024)
 '(hl-paren-background-colors nil)
 '(hl-paren-colors (quote ("yellow1" "gold1" "gold2" "gold3" "gold4")))
 '(initial-frame-alist nil)
 '(mac-command-modifier (quote super))
 '(mac-option-modifier (quote meta))
 '(nrepl-message-colors (quote ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))))

(custom-theme-set-faces
 'el-zenburn
 '(fringe ((t (:background "#3f3f3f" :foreground "#DCDCCC"))))
 '(header-line ((t (:background "#2B2B2B" :foreground "#F0DFAF" :box nil))))
 '(linum ((t (:foreground "gray45" :box nil :underline nil))))
 '(mode-line ((t (:background "dark slate gray" :foreground "#8FB28F" :box nil :overline t))))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:background "slate gray" :foreground "#5F7F5F" :box nil))))
 '(region ((t (:inverse-video t))))
 '(show-paren-mismatch ((t (:background "#6F6F6F" :foreground "red" :weight bold))))
 '(tool-bar ((t (:background "grey75" :foreground "black" :box nil))))
 '(variable-pitch ((t (:family "Lato"))))
 '(vertical-border ((t (:foreground "slate gray"))))
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil ...)))))

(provide-theme 'el-zenburn)
