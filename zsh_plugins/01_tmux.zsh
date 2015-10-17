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

vman() {
  if [[ x"" != x"$TMUX" ]]; then
    vsp man $*
  else
    man $*
  fi
}
