set -o vi
bindkey -v

zle -N zle-line-init
zle -N zle-keymap-select

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

bindkey -M vicmd '?' history-incremental-search-backward
bindkey -M viins '^X^e' edit-command-line
bindkey -M viins '^I' complete-word
bindkey -M viins '^Oc' _correct_word
bindkey -M viins '^O?' _complete_debug

# detecting the terminal to get the keybindings right
# TODO: check if obsoleted by other things
if [[ -z $BINDKEYS ]]; then
    # echo 'defining bindkeys in zshrc'
    BINDKEYS=${TERM%-256color}
    BINDKEYS=${BINDKEYS%-noit}
fi

if [[ $BINDKEYS == "screen" ]]; then
    bindkey '[D' backward-word
    bindkey '[C' forward-word
    bindkey '[1~' beginning-of-line
    bindkey '[4~' end-of-line
else
    bindkey -M viins '[1;5D' backward-word
    # bindkey '[5D' backward-word
    bindkey -M viins '[1;5C' forward-word
    # bindkey '[5C' forward-word
    bindkey -M viins 'OH' beginning-of-line
    bindkey -M viins 'OF' end-of-line
fi
bindkey '[3~' delete-char

