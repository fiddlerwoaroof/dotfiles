# Requires pandoc

man-() {
  (echo ".ll 18.0i"; echo ".nr LL 18.0i"; /bin/cat) | /usr/bin/tbl | /usr/bin/groff -Wall -mtty-char -Tascii -mandoc -c
}

2man() {
  input="$2"
  if [[ "$input" == "" ]]; then
    input="-"
  fi
  pandoc -s -f "$1" -t man "$input" 
}

alias html2man='2man html'
alias md2man='2man markdown'
alias latex2man='2man latex'

_pandoc_view() {
  2man $1 $2 | man -l -
}

pandoc_view() {
  man_fun="man -l -"
  _pandoc_view $*
}

dump_html() {
  pandoc_view html "$1"
}

dump_md() {
  pandoc_view markdown "$1"
}

vpandoc_view() {
  man_fun=vman
  _pandoc_view $*
}

vdump_html() {
  vpandoc_view html "$1"
}

vdump_md() {
  vpandoc_view markdown "$1"
}


