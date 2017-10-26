#!/usr/bin/osascript
on run argv
  set currentApp to my getCurrentApp()
  do shell script "/usr/local/bin/emacsclient -c " & (item 1 of argv)
  activate application currentApp
end run

to getCurrentApp()
  return (path to frontmost application as text)
end getCurrentApp
