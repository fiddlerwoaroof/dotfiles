#:depends-on:nix

declare -T NIX_INFO nix_info ':'
for p in ${=NIX_PROFILES}; do
  if [[ -d "$p"/share/info ]]; then
    nix_info=("${nix_info[@]}" "$p"/share/info)
  fi
done
export INFOPATH="$HOME/info":"$NIX_INFO"

NIX_MAN="${${=NIX_PROFILES}[2]}"/share/man
export MANPATH=/opt/local/share/man:"$NIX_MAN":/Applications/Xcode.app/Contents/Developer/usr/share/man:"$MANPATH":/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/usr/share/man:/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man

if [[ -d "$HOME/.zsh_help" ]]; then
    export HELPDIR="$HOME/.zsh_help"
elif [[ -d "$HOME"/.nix-profile/share/zsh/"$(zsh --version | cut -f2 -d' ')"/help ]]; then
    export HELPDIR="$HOME"/.nix-profile/share/zsh/"$(zsh --version | cut -f2 -d' ')"/help
fi
alias run-help >/dev/null && unalias run-help
