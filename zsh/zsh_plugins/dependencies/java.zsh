#:depends-on:utils
if [[ -x /usr/libexec/java_home ]]; then
export JAVA_HOME="`/usr/libexec/java_home`"
elif [[ -d /usr/lib/jvm/ ]]; then
export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-amd64/
fi

prepend_exe_path "$JAVA_HOME/bin"
