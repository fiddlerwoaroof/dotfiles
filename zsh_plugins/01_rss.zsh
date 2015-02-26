# Requires my pandoc functions
rss_firststory() {
  rss2json "$1" | jq -r '.[].entries[0].content[].value' | dump_html -
}
