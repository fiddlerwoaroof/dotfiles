TODO_DIR="$HOME/.todos"

if [[ ! -d "$TODO_DIR" ]]; then
  mkdir -p "$TODO_DIR"/{shells,archive}
fi

ds() {
  local task
  if [[ $# > 0 ]]; then
    task="$@"
  else
    read -r  task\?'What doing? '
  fi
  printf '%s' "$task" > "$TODO_DIR/shells/$$.txt"
}

td() {
  local task
  if [[ $# > 0 ]]; then
    task="$@"
  else
    read -r  task\?'What doing? '
  fi
  printf '%s' "$task" > "$TODO_DIR/current"
}

pop-todo() {
  local shell_file day_file
  shell_file="$TODO_DIR/shells/$$.txt"
  day_file="$TODO_DIR/current"

  if [[ -f "$shell_file" ]]; then
    mv "$shell_file" "$TODO_DIR/archive/$$.$(date +"%Y-%m-%d--%H-%M-%S")"
    return
  fi

  if [[ -f "$day_file" ]]; then
    mv "$day_file" "$TODO_DIR/archive/current.$(date +"%Y-%m-%d--%H-%M-%S")"
    return
  fi
}

doing() {
  local shell_file day_file
  shell_file="$TODO_DIR/shells/$$.txt"
  day_file="$TODO_DIR/current"

  if [[ -f "$shell_file" || -f "$day_file" ]]; then
    printf '\033[0;33mTODO: \033[0m'
  fi

  if [[ -f "$shell_file" ]]; then
    cat "$shell_file" | tr '\n' ' '
  elif [[ -f "$day_file" ]]; then
    cat "$day_file" | tr '\n' ' '
  fi

  if [[ -f "$shell_file" || -f "$day_file" ]]; then
    echo
  fi
}
