#:depends-on:utils
function jenv() {
  export JAVA_HOME="$(/usr/libexec/java_home -v "$1")"
  prepend_exe_path "$JAVA_HOME/bin"
}

if [[ -x /usr/libexec/java_home ]]; then
  jenv 11
elif [[ -d /usr/lib/jvm/ ]]; then
  export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64/
fi
