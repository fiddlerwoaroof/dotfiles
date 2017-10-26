#:depends-on:path-setup
if command -v emacsclient.applescript 2>&1 >/dev/null; then
    export VISUAL="emacsclient.applescript"
elif command -v emacsclient 2>&1 >/dev/null; then
  cat > "$HOME/bin/newemacs" <<"EOF"
#!/bin/sh
emacsclient -c "$@"
EOF
  chmod +x "$HOME/bin/newemacs"
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
