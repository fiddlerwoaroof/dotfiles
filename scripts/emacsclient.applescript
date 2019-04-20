#!/usr/bin/osascript
on run argv
  set currentApp to my getCurrentApp()
  do shell script "/usr/local/bin/emacsclient -c '" & (item 1 of argv) & "'"
  do shell script "open '/Volumes/" & currentApp & "'"
end run

to getCurrentApp()
  return replace_chars((path to frontmost application as text), ":", "/")
end getCurrentApp

on replace_chars(this_text, search_string, replacement_string)
 set AppleScript's text item delimiters to the search_string
 set the item_list to every text item of this_text
 set AppleScript's text item delimiters to the replacement_string
 set this_text to the item_list as string
 set AppleScript's text item delimiters to ""
 return this_text
end replace_chars


