vspf() {
  tmux split-window -h "$*"
}
vsp() {
  tmux split-window -d -h "$*"
}
sp() {
  tmux split-window -d "$*"
}
spf() {
  tmux split-window "$*"
}

_vman_helper() {
  inp="`mktemp -u`"
  mkfifo "$inp"
  echo "$inp"
  vsp man -l "$inp"
}

vman() {
  if [[ x"$TMUX" != x"" ]]; then
    if [[ x"$1" == "x" ]]; then
      cat - > `_vman_helper`
    else
      vsp man $*
    fi
  else
    man $*
  fi
}

