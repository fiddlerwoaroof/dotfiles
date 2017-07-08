set -o vi
bindkey -M vicmd '?' history-incremental-search-backward
bindkey '^X^e' edit-command-line
bindkey '^I' complete-word
bindkey -M viins '^Oc' _correct_word
bindkey -M viins '^O?' _complete_debug

# detecting the terminal to get the keybindings right
# TODO: check if obsoleted by other things
if [[ -z $BINDKEYS ]]; then
    # echo 'defining bindkeys in zshrc'
    BINDKEYS=${TERM%-256color}
    BINDKEYS=${BINDKEYS%-noit}
fi

bindkey -e
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

