FORTUNE="$(command -v fortune)"
if [ -x "$FORTUNE" ]; then
  $FORTUNE -ec
fi

#zprof

export NVM_DIR="/home/edwlan/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

echo 'zshrc done'

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/edwlan/.gvm/bin/gvm-init.sh" ]] && source "/Users/edwlan/.gvm/bin/gvm-init.sh"

echo plugins done
