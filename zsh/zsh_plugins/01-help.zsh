#:depends-on:nix

NIX_INFO="${${=NIX_PROFILES}[2]}"/share/info
export INFOPATH="$HOME/info":"$NIX_INFO"

NIX_MAN="${${=NIX_PROFILES}[2]}"/share/man
export MANPATH=/opt/local/share/man:"$NIX_MAN":/Applications/Xcode.app/Contents/Developer/usr/share/man:"$MANPATH":/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/usr/share/man:/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man

if [[ -d "$HOME/.zsh_help" ]]; then
    export HELPDIR="$HOME/.zsh_help"
fi
alias run-help >/dev/null && unalias run-help
