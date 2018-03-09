man-() {
  (echo ".ll 18.0i"; echo ".nr LL 18.0i"; /bin/cat) | /usr/bin/tbl | /usr/bin/groff -Wall -mtty-char -Tascii -mandoc -c
}

vspf() {
  tmux split-window -h "$*; sleep 1"
}
vsp() {
  tmux split-window -d -h "$*; sleep 1"
}
sp() {
  tmux split-window -d "$*; sleep 1"
}
spf() {
  tmux split-window "$*; sleep 1"
}

pager() {
  $PAGER -f "$*"
}

_vman_helper() {
  inp="`mktemp -u`"
  mkfifo "$inp"
  echo "$inp"
  vsp man- "$inp"
}

vman() {
  if [[ x"$TMUX" != x"" ]]; then
    if [[ x"$1" == "x" ]]; then
      cat - | man- > `_vman_helper`
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
  vsp ${PAGER:-less} -f "$inp"
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
