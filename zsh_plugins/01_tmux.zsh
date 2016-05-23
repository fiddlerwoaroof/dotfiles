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

_vman_helper() {
  inp="`mktemp -u`"
  mkfifo "$inp"
  echo "$inp"
  vsp man -l "$inp"
}

vman() {
  if [[ x"$TMUX" != x"" ]]; then
    if [[ x"$1" == "x" ]]; then
      cat - > `_vman_helper`
    else
      vsp man $*
    fi
  else
    man $*
  fi
}

_vless_helper() {
  inp="`mktemp -u`"
  mkfifo "$inp"
  echo "$inp"
  vsp $PAGER "$inp"
}

vless() {
  if [[ x"$TMUX" != x"" ]]; then
    if [[ x"$1" == "x" ]]; then
      cat - > `_vless_helper`
    else
      vsp ${PAGER:-less} "$@"
    fi
  else
    ${PAGER:-less} "$@"
  fi
}

tmux_ps() {
  (for s in `tmux list-sessions -F '#{session_name}'` ; do
    echo -e "\ntmux session name: $s\n--------------------"
    for p in `tmux list-panes -s -F '#{pane_pid}' -t "$s"` ; do
      pstree -p -a -A $p
    done
  done) | eval $PAGER
}
