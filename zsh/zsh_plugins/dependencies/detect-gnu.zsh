export GNU_PREFIX=''
platform="$(uname -s)"
echo the platform is "$platform"
if [[ $platform == 'Darwin' ]] && command -v gls 2>&1 >/dev/null; then
	GNU_PREFIX='g'
else
   echo gnu tools not found
fi
