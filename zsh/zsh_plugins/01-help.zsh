export INFOPATH=/usr/local/share/info:/usr/local/texlive/2019/texmf-dist/doc/info
export MANPATH="/opt/local/share/man:/Applications/Xcode.app/Contents/Developer/usr/share/man:$MANPATH"

if [[ -d "$HOME/.zsh_help" ]]; then
    export HELPDIR="$HOME/.zsh_help"
fi
alias run-help >/dev/null && unalias run-help
bindkey -v '^_' run-help

