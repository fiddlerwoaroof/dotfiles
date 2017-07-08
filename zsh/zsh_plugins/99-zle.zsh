# We want the up + down arrows to do completion
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey -M viins "^[[A" up-line-or-beginning-search # Up
bindkey -M vicmd "^[[A" up-line-or-beginning-search # Up
bindkey -M viins "^[[B" down-line-or-beginning-search # Down
bindkey -M vicmd "^[[B" down-line-or-beginning-search # Down
bindkey -M vicmd "k" up-line-or-beginning-search # Up
bindkey -M vicmd "j" down-line-or-beginning-search # Down
