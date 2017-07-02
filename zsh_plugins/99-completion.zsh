# # zstyle ':completion:*' add-space true
# # zstyle ':completion:*' insert-unambiguous true
# zstyle ':completion:*' completer _list _oldlist _expand _complete _match _correct _prefix
# zstyle ':completion:*' completions 1
# zstyle ':completion:*' expand prefix suffix
# zstyle ':completion:*' file-sort name
# zstyle ':completion:*' format 'Completing %D %d'
# zstyle ':completion:*' glob 1
# zstyle ':completion:*' group-name ''
# zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]} r:|[-._\ ]=** r:|=**' '+m:{[:lower:]}={[:upper:]} r:|[-._\ ]=** r:|=**' '+m:{[:lower:]}={[:upper:]} r:|[-._\ ]=** r:|=** l:|=*' '+m:{[:lower:]}={[:upper:]} r:|[-._\ ]=** r:|=** l:|=*'
# zstyle ':completion:*' max-errors 1
# zstyle ':completion:*' prompt '%e errors:'
# zstyle ':completion:*' substitute 1
# zstyle :compinstall filename '/Users/elangley/.zshrc'
# export fpath=($HOME/.zsh.d/completion $fpath)

# The following lines were added by compinstall


# zstyle ':completion:*' completer _expand _complete #_match _prefix
zstyle ':completion:*' completer _list _oldlist _expand _complete _match _prefix #_correct _prefix
zstyle ':completion:*' keep-prefix true
zstyle ':completion:*' format 'Completing %D %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[-._\ ]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[-._\ ]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[-._\ ]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[-._\ ]=** r:|=**'
# zstyle ':completion:*' match-original both
zstyle ':completion:*' menu select=2
# zstyle ':completion:*' original true
zstyle ':completion:*' original false
zstyle ':completion:*' prompt '%e errors:'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*' show-ambiguity true
zstyle ':completion:*' use-compctl false
zstyle ':completion::complete:cd::' group-order directory-stack local-directories named-directories users
zstyle :compinstall filename '/Users/elangley/.zsh.d/99-completion.zsh'


source $HOME/.zsh.d/nix-zsh-completions/nix.plugin.zsh
fpath=($HOME/.zsh.d/completion $fpath $HOME/.zsh.d/nix-zsh-completions)
autoload -U compinit && compinit

compinit

# # End of lines added by compinstall
