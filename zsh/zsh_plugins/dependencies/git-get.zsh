GIT_3DP_DIR="${GIT_3DP_DIR:-"$HOME/git_repos/3dp"}"
GITHUB_USE_SSH=${GIT_USE_SSH:-${GITHUB_USE_SSH:-yes}}
GITLAB_USE_SSH=${GIT_USE_SSH:-${GITLAB_USE_SSH:-yes}}
typeset -g -A FORGE_ALIASES

function initialize-git-get() {
  set -x
  mkdir -p "$GIT_3DP_DIR"
  set +x
}

local alias_usage="alias_forge <alias> <forge>"
function alias_forge() {
  local alias=${1:?the first parameter to alias_forge should be the alias you want to set}
  local forge=${2:?the second parameter to alias_forge should be the forge the alias points to}

  if (( $# > 2 )); then
    echo "$alias_usage"
  else
    FORGE_ALIASES[$alias]=$forge
  fi
}

function github_url() {
  local git_spec="$package"

  if [[ -n "$git_user" ]]; then
    git_spec="$git_user/$package"
  fi

  if [[ "$GITHUB_USE_SSH" == "yes" ]]; then
    printf "git@github.com:%s.git" "$git_spec"
  else
    printf "https://github.com/%s.git" "$git_spec"
  fi
}

function bitbucket_url() {
  local git_spec="$package"

  if [[ -n "$git_user" ]]; then
    git_spec="$git_user/$package"
  fi

  if [[ "$bitbucket_USE_SSH" == "yes" ]]; then
    printf "git@bitbucket.com:%s.git" "$git_spec"
  else
    printf "https://bitbucket.com/%s.git" "$git_spec"
  fi
}

function gitlab_url() {
  local git_spec="$package"

  if [[ -n "$git_user" ]]; then
    git_spec="$git_user/$package"
  fi

  if [[ "$GITLAB_USE_SSH" == "yes" ]]; then
    printf "git@gitlab.com:%s.git" "$git_spec"
  else
    printf "https://gitlab.com/%s.git" "$git_spec"
  fi
}

function git-forge-clone() {
  if [[ ! -e "$(basename "$package")" ]]; then
    git clone "$($1)"
  else
    echo "package already cloned"
  fi
}

function get_forge_function() {
  local forge="$1"
  local aliased_forge="${forge_aliases[$1]}"
  if [[ ! -z "$aliased_forge" ]]; then
    forge="$aliased_forge"
  fi
  echo "${forge}_url"
}

function get_forge_root() {
  local forge="$1"
  if command -v "${forge}_root" &>>-; then
    "${forge}_root"
  else
    echo "$GIT_3DP_DIR"/
  fi
}

alias_forge bb github
alias_forge gh github
alias_forge gl gitlab

function git-get() {
  local git_user
  local package
  
  forge=${1?Need a forge spec}

  shift 

  if [[ $# == 1 ]]; then
    git_user=
    package=$1
  elif (( $# == 2 )); then
    git_user=$1
    package=$2
    shift
  else
    echo 'usage: <forge> <user> <package>'
    return 2
  fi

  package=${package%.git}

  shift

  local target="$(get_forge_root "$forge")"
  cd "$target"

  local forge_url_function="${$(get_forge_function "$forge"):?forge not recognized}"
  git-forge-clone "$forge_url_function"
  
  cd "$(basename "$package")"
}

