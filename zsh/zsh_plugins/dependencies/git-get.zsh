GIT_3DP_DIR="${GIT_3DP_DIR:-"$HOME/git_repos/3dp"}"

typeset -g -A FORGE_ALIASES

alias_usage="alias_forge <alias> <forge>"
function alias_forge() {
  local alias=${1:?the first parameter to alias_forge should be the alias you want to set}
  local forge=${2:?the first parameter to alias_forge should be the forge the alias points to}

  if (( $# > 2 )); then
    printf "%s" "$alias_usage"
  else
    FORGE_ALIASES[$alias]=$forge
  fi
}

alias_forge gh github
alias_forge gl gitlab

GITHUB_USE_SSH=${GIT_USE_SSH:-${GITHUB_USE_SSH:-yes}}
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

GITLAB_USE_SSH=${GIT_USE_SSH:-${GITLAB_USE_SSH:-yes}}
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
    echo 'usage: <forge> <user> <package> or <forge> <package>'
    return 2
  fi

  package=${package%.git}

  shift

  cd "$GIT_3DP_DIR"

  local forge_url_function="${$(get_forge_function "$forge"):?forge not recognized}"
  git-forge-clone "$forge_url_function"
  
  cd "$(basename "$package")"
}
