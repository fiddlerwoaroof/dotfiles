git-root() {
  cd `git rev-parse --show-toplevel`
}
alias groot=git-root

is-function () {
  whence -w $1 | grep --color=auto -I function > /dev/null
}

GIT_CMD="`which git 2>/dev/null`"

if [[ "$GIT_CMD"x != ""x ]]; then
  # git wrapper that mimics the functionality of git for commandlets but also
  # searches shell functions.
  g() {
    POSSIBLE_CMD="git-$1"
    if is-function $POSSIBLE_CMD; then
      $POSSIBLE_CMD "${@[2,-1]}"
    else
      git "$@"
    fi
  }
fi
