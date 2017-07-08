#:depends-on:nix
#:depends-on:autoloads
#:depends-on:sh-opts
#:depends-on:detect-gnu
#:depends-on:key-bindings
#:depends-on:editor-setup

# echo "begin zshrc"
echo "shell session started for $USER on $TTY at `date`" | tee /dev/stderr | logger

source $HOME/.localzshrc.sh

export VIMCLOJURE_SERVER_JAR="$HOME/bin/jars/server-2.3.6.jar"

export PAGER="less -SiemX"

export RGBDEF='/opt/X11/share/X11/rgb.txt'
export GREP_COLORS='ms=01;31:mc=01;31:sl=:cx=:fn=35:ln=32:bn=32:se=36'

#------------------

alias :w='cat >'

alias "cd-"="cd -"
alias jmp=pushd
alias ret=popd
alias grep="grep --color=auto -I"
alias la="ls -AF"
alias ,=pydit
alias cvsdiff='cvs diff -wbB | colordiff'
alias cp.="${GNU_PREFIX}cp --target-directory=."
alias find='noglob find'

showspaces() {
    python -c'import sys;print sys.stdin.read().replace(" ","_").replace("\t", "----")'
}

truncfile() {
    echo $1
    (( number = $1 - 1 ))
    ${GNU_PREFIX}sed -i "${number}q" $2
}


savedir() {
    cmd="export "`basename $PWD`"=$PWD"
    eval $cmd
    truncfile 50 $HOME/.gtaliases
    echo $cmd >> $HOME/.gtaliases
}

hash -d "desktop=$HOME/Desktop"
hash -d "downloads=$HOME/Downloads"
hash -d "programming=$HOME/Programming"
gtde() { cd ~desktop/"${(j</>)*}" }
gtdo() { cd ~downloads/"${(j</>)*}" }
gtp() {cd ~programming/"${(j</>)*}" }
gt() { cd ~"${(j</>)*}" }
[[ -e $HOME/.gtaliases ]] || touch $HOME/.gtaliases

gta() {
    cmd="hash -d \"$1\"=\"$PWD/${(j</>)*}\""
    eval $cmd
    gt $1
    truncfile 50 $HOME/.gtaliases
    echo $cmd >> $HOME/.gtaliases
}

gtmpdir() {
  cd "$(mktemp -d "$@")"
}

source $HOME/.gtaliases
gthaskell() { gtp haskell }
gtprolog() { gtp prolog }
gtpython() { gtp python }

despace() { echo ${1// /} }
escape() { echo ${(j<\\ >)*} }
archive() {
    if [ ! -d .bak ]; then
	mkdir .bak
    fi
    FN=".bak/${1// /}-`date +"%Y%m%d.%H%M%S"`.tbz"
    echo -n archiving $FN...
    tar jhcf $FN $1
    echo done.
}

editrc() {
    hash=`md5 $HOME/.zshrc`
    $VISUAL $HOME/.zshrc
    newhash=`md5 $HOME/.zshrc`
    if [[ $hash != $newhash ]]; then
	source $HOME/.zshrc
    fi
}

rl() { source $HOME/.zshrc }

getlink() { #gtdo
    curl "`pbpaste`" > $(basename `pbpaste`)
}

copypwd() { echo -n `pwd` | pbcopy }
alias sdir='copypwd'

sshto() {
    TARGET=`egrep "^$1:" ~/.ssh_dests | cut -d: -f2`
    USER=`grep $TARGET ~/.ssh_dests | cut -d: -f3`
    ssh $USER@$TARGET
}

dirsave() {
    pwd | ctext
}
dirgo() {
    cd `ptext`
}

ccwd() {
    pwd | ucopy
}

gdir() {
    cd `upaste`
}

ulimit -c unlimited
zle -N edit-command-line

add_to_sandbox() {
    echo adding $1 to sandbox
    cp "$1" "$HOME/sandbox"
    cd "$HOME/sandbox"
    git add "`basename $1`"
    git commit -a -m "added snippet $1"
    cd -
}

wiki() {
    pushd $HOME/mywiki > /dev/null
    soywiki
    popd > /dev/null
}

es() {
    python2.7 -c "e('$1')"
}

load_snippet() {
    python -ic "import sitecustomize;ls('$1')"
}

alias page="$PAGER"
export VIRTUALENV=/usr

export PYTHONPATH="$PYTHONPATH":"$HOME/pythonlibs"

getshelljobtrees() {
    pstree `pgrep '^login$'`
}


psgrep() {
    ps auxw | grep --color=yes $* | grep -v grep --color=no
}

dis() {
    jobs
    echo -n 'disown which? '
    n=-1
    read n
    if [[ ! -z "$n" ]]; then
	disown %$n
    fi
}

alias dq=dmenu_queue_mpd
alias dqp=dmenu_queueplay_mpd

export GOPATH=$HOME/go

rvm use system &>/dev/null

export CPATH=$CPATH:$HOME/include
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/lib
export VIMCLOJURE_SERVER_JAR="$HOME/lib/vimclojure/server-2.3.6.jar"
export CLON_THEME=el
