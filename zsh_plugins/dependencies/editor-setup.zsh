if command -v emacsclient 2>&1 >/dev/null; then
  function newemacs() {
    emacsclient -c "$@"
  }
  export VISUAL=newemacs
elif command -v vim 2>&1 >/dev/null; then
  export VISUAL=$(which vim)
fi

export EDITOR=vim

vim() {
    stty -ixon
    env vim $*
    stty ixany
}

function :e {
  $VISUAL "$@"
}
alias vi='vim'

alias v=$VISUAL
alias e=$EDITOR
