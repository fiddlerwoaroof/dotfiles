getpspwds () {
	(
		for x in /proc/*/cwd
		do
			echo "$x" `readlink "$x";` \"`cat $(dirname $x)/cmdline`\"
		done
	) 2> /dev/null | gawk -vFPAT='([^[:space:]]*)|"([^"]*)"' -vOFS=: 'NF == 3 { $1=$1; print $2 OFS $1 OFS $3 }' | column -s: -t | less -S
}
