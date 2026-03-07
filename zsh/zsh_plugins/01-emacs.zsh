export ALTERNATE_EDITOR=vim
export FWOAR_REAL_EMACSCLIENT="$(command -v emacsclient)"
emacsclient() (
  unset TMPDIR
  "$FWOAR_REAL_EMACSCLIENT" "$@"
)

semacs() {
  emacs --eval "(setq server-name \"$1\")" --daemon
}
