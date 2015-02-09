# Requires my pandoc functions
rss_firststory() {
  rss2json "$1" | jq '.[].entries[0].content[].value' | json-destringify  | dump_html -
}
