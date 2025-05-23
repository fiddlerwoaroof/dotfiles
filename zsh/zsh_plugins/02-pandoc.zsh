# Requires pandoc

2man() {
  local input="$2"
  if [[ "$input" == "" ]]; then
    input="-"
  fi
  pandoc -s -f "$1" -t man "$input" 
}

alias html2man='2man html'
alias md2man='2man markdown'
alias latex2man='2man latex'
vmd() {
  md2man $1 | man- | less -R
}

_pandoc_view() {
  2man $1 $2 | man-
}

pandoc_view() {
  man_fun="man-"
  _pandoc_view $*
}

dump_html() {
  pandoc_view html "$1"
}

dump_md() {
  pandoc_view markdown "$1"
}

vpandoc_view() {
  man_fuvman
  _pandoc_view $*
}

vdump_html() {
  vpandoc_view html "$1"
}

vdump_md() {
  vpandoc_view markdown "$1"
}


