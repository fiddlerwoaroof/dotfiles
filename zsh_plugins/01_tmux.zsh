vspf() {
  tmux split-window -h "$*"
}
vsp() {
  tmux split-window -d -h "$*"
}
