#:depends-on:utils
#:depends-on:nix
#:depends-on:java
###

orig_paths=("$HOME"/bin "${path[@]}")
for fw_p in "$orig_paths[@]"; do
  push_exe_path "$fw_p"
done

push_exe_path "$JAVA_HOME"/bin
