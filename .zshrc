echo "begin zshrc"
echo ".zshrc loaded for $USER on $TTY at `date`" | logger

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git ruby rails osx brew zsh-syntax-highlighting python)

source $ZSH/oh-my-zsh.sh
unsetopt correct_all

echo "done oh-my-zsh"

# Customize to your needs...
export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/texbin:/usr/X11/bin:/Users/edwlan/.rvm/gems/ruby-1.9.2-p180/bin:/Users/edwlan/.rvm/gems/ruby-1.9.2-p180@global/bin:/Users/edwlan/.rvm/rubies/ruby-1.9.2-p180/bin:/Users/edwlan/.rvm/bin:/opt/local/bin:/sbin/usr/sbin:/Users/edwlan/.cabal/bin:/Users/edwlan/bin:/Developer/usr/bin
#source /usr/local/Cellar/coreutils/8.12/aliases
#unalias kill

SED=/usr/local/bin/gsed
if [[ -e /etc/sysconfig/zsh-prompt-$TERM ]]; then
  . /etc/sysconfig/zsh-prompt-$TERM 
fi

if [ -x /opt/local/bin/fortune ]; then export FORTUNE=/opt/local/bin/fortune
elif [ -x /usr/local/bin/fortune ]; then export FORTUNE=/usr/local/bin/fortune
elif [ -x /usr/games/fortune ]; then export FORTUNE=/usr/games/fortune
else export FORTUNE=/usr/bin/fortune
fi

$FORTUNE

export PYTHONSTARTUP=$HOME/Library/Python/2.7/site-packages/sitecustomize.py
PS1="---
(%?) %m:%n--%l %/
%!:%# "
export PS1
RPROMPT="[%T]"
export RPROMPT
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
PATH=/home/edwlan/bin:/usr/local/bin:$PATH
export PATH="/opt/local/bin:/usr/sbin:/sbin/usr/sbin:/sbin:$HOME/.cabal/bin:$HOME/bin:/Developer/usr/bin:$PATH"
export VIMCLOJURE_SERVER_JAR="$HOME/bin/jars/server-2.3.6.jar"
export INFOPATH=/usr/local/share/info:/usr/local/texlive/2009/texmf/doc/info

export SAVEHIST=10000000
export HISTSIZE=10000000
export HISTFILE=$HOME/.zshistory

export PKG_CONFIG_PATH="$PKG_CONFIG_PATH"

export MANPATH="/opt/local/share/man:$MANPATH"

export PAGER="/bin/sh -c \"unset PAGER;col -b -x | \
    vim -R -c 'set ft=man nomod nolist' -c 'map q :q<CR>' \
    -c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
    -c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""

export RGBDEF='/opt/X11/share/X11/rgb.txt'

if [ -x /usr/local/bin/vim ]; then
   export VISUAL="/usr/local/bin/vim"
   export EDITOR="/usr/local/bin/vim"
   export PAGER="col -b | /usr/local/bin/vim -u ~/.vimrc.more -"
else
   export VISUAL="/usr/bin/vim"
   export PAGER="col -b | /usr/bin/vim -u ~/.vimrc.more -"
fi

#alias run-help > /dev/null && unalias run-help
#alias help=run-help
#------------------
autoload run-help
autoload -U zfinit
autoload -U tcp_proxy
autoload -U tcp_open
autoload -U tcp_point
autoload -U tcp_shoot
#------------------
zfinit
#if [ $TERM != 'dumb' ]; then
#  eval `dircolors -b`
#  alias ls='ls --color=auto'
#f
setopt autopushd
setopt cdablevars
setopt AUTO_LIST
setopt LIST_PACKED
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_VERIFY
setopt noBG_NICE
setopt PUSHD_IGNORE_DUPS
setopt autocd
setopt chaselinks
setopt markdirs
# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete #_match _prefix
zstyle ':completion:*' format 'Completing %D %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=**'
zstyle ':completion:*' menu select=0
zstyle ':completion:*' original false
zstyle ':completion:*' prompt '%e errors:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle :compinstall filename '/Users/edwlan/xxx.zsh'

autoload -Uz compinit

compinit
# End of lines added by compinstall

bindkey -e
bindkey '[1;5D' backward-word
bindkey '[5D' backward-word
bindkey '[1;5C' forward-word
bindkey '[5C' forward-word
bindkey 'OH' beginning-of-line
bindkey 'OF' end-of-line
bindkey '[3~' delete-char

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function

alias vi='vim'
alias :e='vim'
alias :w='cat >'

alias "cd-"="cd -"
#alias "ls"="gls --color=auto -F"
#alias "lsa"="ls -AF"
alias poty=port
alias jmp=pushd
alias ret=popd
alias ..python="PYTHONPATH=.. python"
alias .python="PYTHONPATH=. python"
alias cvs="cvs -q"
alias cvsu="cvs -q update -P"
alias cvsud="cvs -q update -dP"
alias grep="grep --color=auto -I"
alias -g .cf="grep -r '<<<' * | grep \.py | grep -vi binary | cut -d: -f1"
alias la="ls -A"
alias ,=pydit
alias tw=twitter_tool
alias v=$VISUAL
alias e=$EDITOR
alias cvsdiff='cvs diff -wbB | colordiff'
alias cp.='gcp --target-directory=.'
alias notep='note post'

echo "done variables and options"

noteg() {
  note get "$*"
}
alias notel='note list'
alias clipnote='pbpaste | note post'

showspaces() {
	python -c'import sys;print sys.stdin.read().replace(" ",".").replace("\t", "—---")'
}

cvsc() {
    FN=$1
    shift
    cvs -q commit -m "'$*'" $FN 
}
alias cvsc.="cvsc ."

truncfile() {
   echo $1
   (( number = $1 - 1 ))
   $SED -i "${number}q" $2
}
gtde() { cd $HOME/Desktop/$1 }
gtdo() { cd $HOME/Downloads/$1 }
gtp() {cd $HOME/Programming/$1 }
gt() { cd $HOME/$1 }
if [[ ! -e $HHOME/.gtaliases ]]; then touch $HOME/.gtaliases; fi

savedir() {
  cmd="export "`basename $PWD`"=$PWD"
  eval $cmd
  truncfile 50 $HOME/.gtaliases
  echo $cmd >> $HOME/.gtaliases
}

gtpa() {
  gtp $1
  cmd="alias \"gt_$1\"=\"gtp $1\""
  eval $cmd
  truncfile 50 $HOME/.gtaliases
  echo $cmd >> $HOME/.gtaliases
}
gta() {
  gt $1
  cmd="alias \"gt_$1\"=\"gt $1\""
  eval $cmd
  truncfile 50 $HOME/.gtaliases
  echo $cmd >> $HOME/.gtaliases
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
    FN=.bak/`despace $1`-`date +"%Y%m%d.%H%M%S"`.tbz 
    echo -n archiving $FN...
    tar jhcf $FN $1
    echo done.
}
editrc() {
  $VISUAL $HOME/.zshrc
  source $HOME/.zshrc
}
rl() { source $HOME/.zshrc }
getlink() { #gtdo
 curl "`pbpaste`" > $(basename `pbpaste`)
 #popd 
 #echo `pbpaste` --> $(basename `pbpaste`)
}
copypwd() { echo -n `pwd` | pbcopy }
alias sdir='copypwd'

sshto() {
    TARGET=`egrep ^$1: ~/.ssh_dests | cut -d: -f2`
    USER=`grep $TARGET ~/.ssh_dests | cut -d: -f3`
    ssh $USER@$TARGET
}

cvscmp() {
    cvs status  | grep File | grep -v "Up-to-date"
}

cvsr() {
	echo removing $1
	rm $1
	cvs remove $1
}

cvsm(){
	echo moving $1 to $2
	mv $1 $2
	cvs remove $1
	cvs add $2/$1
}

addrssitem() {
    cd $HOME/Programming/dirrss
    vi $1
    cd $OLDPWD
}

pathswitch() {
	REMOVE=$1
	REPLACE=$2
	cd ${PWD/$REMOVE/$REPLACE}
}
#debug
ccwd() {
    pwd | pbcopy
}
gdir() {
    cd `pbpaste`
}
ulimit -c unlimited
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^e' edit-command-line

# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
# pip zsh completion end

add_to_sandbox() {
   echo adding $1 to sandbox
   cp $1 $HOME/sandbox
   cd $HOME/sandbox
   git add `basename $1`
   git commit -a -m "added snippet $1"
   cd -
} 

psc() {
   python -c "from __future__ import print_function; import sys;$1"
}

activate_env() {
   pushd $HOME/python_envs/ > /dev/null
   source $1*/bin/activate
   popd > /dev/null
}
alias ae=activate_env

new_virtual_env() {
   virtualenv -p python$2 --no-site-packages --distribute $HOME/python_envs/$1
   pushd $HOME/python_envs/$1
}
alias ne=new_virtual_env

ge() {
   cd $VIRTUAL_ENV
}

messages() {
   egrep --color=yes -IHnro '(TODO|NOTE|FIXME|BUG):.*$' . |
   psc '
for line in sys.stdin:
      line = line.strip().split(":", 2)
      print("%s\n\t%s" % (":".join(line[2:]), ":".join(line[:2])))'
}

wiki() {
   pushd $HOME/mywiki > /dev/null
   $HOME/bin/soywiki
   popd > /dev/null
}

dupfind() {
   gfind \( \( -name .git -o -name CVS \) -prune \) -o  \( -type f \) -print0  | xargs -0 shasum | sort | guniq -w 20 -c | sort -nr
}

es() {
   python2.7 -c "e('$1')"
}

load_snippet() {
   python -ic "import sitecustomize;ls('$1')"
}

alias page=$PAGER
export VIRTUALENV=/usr

setopt allexport

export PERL_LOCAL_LIB_ROOT="/Users/edwlan/perl5";
export PERL_MB_OPT="--install_base /Users/edwlan/perl5";
export PERL_MM_OPT="INSTALL_BASE=/Users/edwlan/perl5";
export PERL5LIB="/Users/edwlan/perl5/lib/perl5/darwin-thread-multi-2level:/Users/edwlan/perl5/lib/perl5";
export PATH="/Users/edwlan/perl5/bin:$PATH";
export PYTHONPATH=$PYTHONPATH:$HOME/pythonlibs

PASSWD_RIGHT=True
cuauth() {
   if [ $PASSWD_RIGHT ]; then
      passwd=`security find-internet-password -l "ntsrva.cua.edu" -w`
   else
      passwd=`prompt_password 69langley`
   fi

   postdata="buttonClicked=4&err_flag=0&err_msg=&info_flag=0&info_msg=&redirect_url=&username=69langley&password=$passwd"
   curl https://wirelessauth.cua.edu/login.html -d $postdata 2>&1 | html2ps | ps2ascii && return
   unset PASSWD_RIGHT
}

getshelljobtrees() {
   pstree `pgrep '^login$'`
}

psgrep() {
   ps auxw | grep --color=yes $* | grep -v grep --color=no
}

echo 'zshrc done'
