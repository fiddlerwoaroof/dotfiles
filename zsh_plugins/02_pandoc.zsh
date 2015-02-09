# Requires pandoc

pandoc_view() {
  input="$2"
  if [[ "$input" == "" ]]; then
    input="-"
  fi
  pandoc -s -f "$1" -t man "$input" | man -l -
}

dump_html() {
  pandoc_view html "$1"
}

dump_md() {
  pandoc_view markdown "$1"
}


