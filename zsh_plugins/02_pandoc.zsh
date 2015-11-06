# Requires pandoc

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

pandoc_view() {
  2man $1 $2 | man -l -
}

dump_html() {
  pandoc_view html "$1"
}

dump_md() {
  pandoc_view markdown "$1"
}


