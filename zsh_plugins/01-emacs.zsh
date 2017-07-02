semacs() {
  emacs --eval "(setq server-name \"$1\")" --daemon
}
