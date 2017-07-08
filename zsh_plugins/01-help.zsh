export INFOPATH=/usr/local/share/info:/usr/local/texlive/2009/texmf/doc/info
export MANPATH="/opt/local/share/man:/Applications/Xcode.app/Contents/Developer/usr/share/man:$MANPATH"

export HELPDIR="$HOME/.zsh_help"
unalias run-help
bindkey -v '^_' run-help
