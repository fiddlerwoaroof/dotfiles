#:depends-on:utils
#:depends-on:nix
export GNU_PREFIX=''
platform="$(uname -s)"
# interactive_echo the platform is "$platform"
if [[ $platform == 'Darwin' ]] && command -v gls 2>&1 >/dev/null; then
	GNU_PREFIX='g'
   GREALPATH='grealpath'
else
   echo gnu tools unprefixed
   GREALPATH='realpath'
fi
