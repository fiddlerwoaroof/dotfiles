#:depends-on:nix

NIX_INFO="${${=NIX_PROFILES}[2]}"/share/info
export INFOPATH="$NIX_INFO":/usr/local/share/info:/usr/local/texlive/2019/texmf-dist/doc/info

NIX_INFO="${${=NIX_PROFILES}[2]}"/share/man
export MANPATH="/opt/local/share/man:/Applications/Xcode.app/Contents/Developer/usr/share/man:$MANPATH"

if [[ -d "$HOME/.zsh_help" ]]; then
    export HELPDIR="$HOME/.zsh_help"
fi
alias run-help >/dev/null && unalias run-help
