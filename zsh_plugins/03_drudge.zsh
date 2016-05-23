# Drudge Report Reader
# requires my rss2json script, my interface to python-goose, pandoc and my rss zsh plugin
check_drudge() {
  rss2json http://www.drudgereportfeed.com/rss.xml | jq  '[.[0].entries[] | {title:.title, link:.link}]' 
}

read_drudge() {
  if [[ "$1" == "1" ]]; then
    check_drudge | jq -r '.[0].link' | xargs goosify | dump_html
    return 0
  fi

  unset choice
  data=`check_drudge`
  cmd='echo $data | jq ".[].title"'
  if [[ "$1" == "" ]]; then
    echo $data | jq '.[].title' | nl | head -n 10
  elif [[ "$1" == "all" ]]; then
    echo $data | jq '.[].title' | nl
  else
    echo $data | jq '.[].title' | nl | head -n "$1"
  fi

  echo -n "your choice? "
  choice=-1
  read choice
  if [[ $choice == "" ]]; then
    return
  fi
  echo $data | jq -r '.[].link' | sed -n "${choice}p" | xargs goosify | dump_html
}
alias current_news='read_drudge 1'
