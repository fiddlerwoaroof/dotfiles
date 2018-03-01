GIT_DEBUG=0

git-update-repos() {
  find . -name .git | (
    while read -r repo; do
      printf "---\n$repo\n";
      git --work-tree="${repo%/*}" --git-dir="$repo" pull --ff-only ;
      printf "\n\n"
    done
  )
}

git-ub() {
  git stash || exit 1
  git pull --rebase
  git stash pop
}

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
  git merge --ff-only
}

git-pff() {
  git pull --ff-only
}

git-root() {
  git rev-parse --show-toplevel
}

git-ag() {
  ag "$@" -- "$(git-root)"
}

git-cd() {
  cd "$(git-root)/$1"
}
alias groot=git-cd

git-graph() {
  git log --graph --format=oneline --decorate "$@"
}
alias gl=git-graph
alias git-l=git-graph
alias git-hist=git-graph
alias g=git

git-messages() {
  if [[ -d .git ]]; then
    echo "Git log messages:"
    git log -n 5 | egrep --color=yes -Io '(TODO|NOTE|FIXME|BUG|DONE):.*$'
  fi

  echo "Messages from files:"
  egrep --color=yes -IHnro '(TODO|NOTE|FIXME|BUG):.*$' . |
    psc '
for line in sys.stdin:
      line = line.strip().split(":", 2)
      print("%s\n\t%s" % (":".join(line[2:]), ":".join(line[:2])))'
}

alias git-msg=git-messages

GIT_CMD="`which -p git 2>/dev/null`"
GTI_CMD="`which -p gti 2>/dev/null`"

if [[ ! -z "$GIT_CMD" ]]; then
  # git wrapper that mimics the functionality of git for commandlets but also
  # searches shell functions.
  git() {
    local POSSIBLE_CMD
    POSSIBLE_CMD="$(expand-alias git-$1)"

    if is-function "$POSSIBLE_CMD"; then
      $POSSIBLE_CMD "${@[2,-1]}"
    else
      "$GIT_CMD" "$@"
    fi
  }
fi
