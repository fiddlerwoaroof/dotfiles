#:depends-on:00-utils
###
PATH="${PATH%%:<<<:*}:${PATH##*:>>>:}"

prepend_exe_path "$HOME/cj-projects/software/bin"
prepend_exe_path "$HOME/cj-projects/software/resin/3.1.8-pro/bin"
prepend_exe_path "$HOME/pfff/bin"
prepend_exe_path "$HOME/Library/Python/2.7/bin"
prepend_exe_path "$HOME/go/bin"
prepend_exe_path "$HOME/.rvm/bin"
prepend_exe_path /Library/Frameworks/Mono.framework/Versions/Current/Commands
prepend_exe_path /usr/local/share/dotnet
prepend_exe_path /bin
prepend_exe_path /usr/bin
prepend_exe_path /Developer/usr/bin
prepend_exe_path /usr/local/bin
prepend_exe_path "$HOME/.cargo/bin"
prepend_exe_path /opt/local/bin
prepend_exe_path /usr/X11/bin
prepend_exe_path "$HOME/vim8/bin"
prepend_exe_path "$HOME/.cabal/bin"
prepend_exe_path "$HOME/.local/bin"
prepend_exe_path /Library/Java/Home/bin
prepend_exe_path "$HOME/.nix-profile/bin"
prepend_exe_path "$HOME/bin"

prepend_exe_path /sbin
prepend_exe_path /usr/sbin
prepend_exe_path /usr/local/sbin
prepend_exe_path /opt/local/sbin
prepend_exe_path "$HOME/.nix-profile/sbin"

PATH="${PATH%:}"
