GIT_DEBUG=0

git-ufgrep() {
  PATTERN="$1"
  if [[ ${PATTERN}x == ""x ]]; then
    PATTERN='.*'
  fi
  PATTERN="${PATTERN//\//\\/}"

  if [[ "$GIT_DEBUG" == 1 ]]; then
    echo "$PATTERN"
  fi

  git status --porcelain | awk '$1 == "??" && $2 ~ /[.]'"$PATTERN"'$/'
}

git-run() {
  (
    DIR="$1"
    shift
    git cd "$DIR"
    exec "$@"
  )
}

git-back() {
  git checkout -
}

git-ff() {
  git pull --ff-only
}

git-root() {
  git rev-parse --show-toplevel
}

git-ag() {
  ag "$@" -- $(git-root)
}

git-cd() {
  cd "$(git-root)/$1"
}
alias groot=git-cd

is-function () {
  whence -w $1 | grep --color=auto -I function > /dev/null
}

GIT_CMD="`which -p git 2>/dev/null`"

if [[ "$GIT_CMD"x != ""x ]]; then
  # git wrapper that mimics the functionality of git for commandlets but also
  # searches shell functions.
  git() {
    POSSIBLE_CMD="git-$1"
    if is-function $POSSIBLE_CMD; then
      $POSSIBLE_CMD "${@[2,-1]}"
    else
      $GIT_CMD "$@"
    fi
  }
fi


