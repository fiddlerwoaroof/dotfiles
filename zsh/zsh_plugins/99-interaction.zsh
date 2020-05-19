#:depends-on:todo
#:depends-on:key-bindings
#:depends-on:named-directories
#:depends-on:aliases
#:depends-on:autoloads

if [[ -e /etc/sysconfig/zsh-prompt-$TERM ]]; then
  . /etc/sysconfig/zsh-prompt-$TERM
elif [[ -e $HOME/.zsh-prompt-$TERM ]]; then
  . $HOME/.zsh-prompt-$TERM
fi

function battery_charge() {
  if [[ -x "$HOME/bin/batcharge.py" ]]; then
    if [[ $LAPTOP[1] == 'y' &&  $TERM != "xterm" ]]; then
      python "$HOME/bin/batcharge.py" 2>/dev/null
    fi
  else
    return 1
  fi
}

zstyle ':vcs_info:*' actionformats \
    '%F{5}%f%s%F{5}%F{3}->%F{5}%F{2}%b%F{3}|%F{1}%a%F{5}%f'
zstyle ':vcs_info:*' formats       \
  '%F{5}%f%s%F{5}%F{3}->%F{5}%F{2}%b%F{5}%f'
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'

zstyle ':vcs_info:*' enable git cvs svn

vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}

zle-keymap-select () {
  if [[  $TERM_PROGRAM == "iTerm.app" && -o interactive ]]; then
    if [ $KEYMAP = vicmd ]; then
      printf "\033[2 q"
    else
      printf "\033[6 q"
    fi
  fi
}

zle-line-init () {
  if [[  $TERM_PROGRAM == "iTerm.app" && -o interactive ]]; then
    zle -K viins
    printf "\033[6 q"
  fi
}

zle -N zle-line-init
zle -N zle-keymap-select

export PYTHONSTARTUP=$HOME/Library/Python/2.7/site-packages/sitecustomize.py

HOSTNAME=`hostname -f`
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'

cmdtermtitle() {
  cmd_name="${(V)1}"
  if [ 'fg' = "${${(z)@}[1]}" ]; then
    cmd_name="${(vV)jobtexts}"
  fi

  if [[ "${TERM%%-*}"x == "screen"x ]]; then
    echo -ne "\033]0;${cmd_name}\007"
  else
    echo -ne "\033]0; ${cmd_name} : ${USER}@$HOSTNAME\007"
  fi

  printf "\033[2 q"
}

if [[ $TERM != "linux" && ${TERM%-color} != "eterm" ]]; then
   add-zsh-hook preexec cmdtermtitle
fi

termtitle() {
  npwd=${PWD/#$HOME/\~}
  if [[ "${TERM%%-*}"x == "screen"x ]]; then
    echo -ne "\033]0;${npwd}\007"
  else
    echo -ne "\033]0;${USER}@$HOSTNAME: ${npwd}\007"
  fi
}

if [[ $TERM != "linux" && ${TERM%-color} != "eterm" ]]; then
   add-zsh-hook precmd termtitle
fi

if battery_charge 2>&1 >/dev/null; then
  PROMPT='---
(%?) %m:%n--%l %~ `vcs_info_wrapper` `battery_charge 2>/dev/null`
%!:%# '
else
  PROMPT=$'---\n$(doing)${$(doing):+\n}(%?) %m:%n--%l %~ `vcs_info_wrapper` `date +"%Y-%m-%d %H:%M:%S"`
%!:%# '
fi
export PROMPT
