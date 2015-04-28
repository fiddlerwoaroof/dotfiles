echo "begin zshrc"
echo ".zshrc loaded for $USER on $TTY at `date`" | logger

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh
export MPD_HOST=srv2.elangley.org

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
#export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
fpath=(~/.zsh.d/completion ~/.zsh.d/functions $fpath)
plugins=(git ruby rails osx brew zsh-syntax-highlighting python git-extra git-flow battery)

source $ZSH/oh-my-zsh.sh
unsetopt correct_all

echo "done oh-my-zsh"

# Customize to your needs...
export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/texbin:/usr/X11/bin:/opt/local/bin:/sbin/usr/sbin:$PATH
#source /usr/local/Cellar/coreutils/8.12/aliases
#unalias kill

SED=/usr/local/bin/gsed
if [[ -e /etc/sysconfig/zsh-prompt-$TERM ]]; then
  . /etc/sysconfig/zsh-prompt-$TERM 
elif [[ -e $HOME/.zsh-prompt-$TERM ]]; then
  . $HOME/.zsh-prompt-$TERM
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
HOSTNAME=`hostname -f`
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'

cmdtermtitle() {
   echo -ne "\033]0;${USER}@$HOSTNAME: $1\007"
}

if [[ $TERM != "linux" ]]; then
   autoload -U add-zsh-hook
   add-zsh-hook preexec cmdtermtitle
fi

termtitle() {
   npwd=${PWD/#$HOME/\~}
   echo -ne "\033]0;${USER}@$HOSTNAME: ${npwd}\007"
} 

if [[ $TERM != "linux" ]]; then
   add-zsh-hook precmd termtitle
fi

PATH=/home/edwlan/bin:/usr/local/bin:$PATH
export PATH="/opt/local/bin:/usr/sbin:/sbin/usr/sbin:/sbin:$HOME/.cabal/bin:$HOME/bin:/Developer/usr/bin:$PATH"
export VIMCLOJURE_SERVER_JAR="$HOME/bin/jars/server-2.3.6.jar"
export INFOPATH=/usr/local/share/info:/usr/local/texlive/2009/texmf/doc/info
export SAVEHIST=10000000
export HISTSIZE=10000000
export HISTFILE=$HOME/.zshistory

export PKG_CONFIG_PATH="$PKG_CONFIG_PATH"

export MANPATH="/opt/local/share/man:$MANPATH"

#export PAGER="/bin/sh -c \"unset PAGER;col -b -x | \
    #vim -R -c 'set ft=man nomod nolist' -c 'map q :q<CR>' \
    #-c 'map <SPACE> <C-D>' -c 'map b <C-U>' \
    #-c 'nmap K :Man <C-R>=expand(\\\"<cword>\\\")<CR><CR>' -\""
export PAGER="less"

export RGBDEF='/opt/X11/share/X11/rgb.txt'
export GREP_COLORS='ms=01;31:mc=01;31:sl=:cx=:fn=35:ln=32:bn=32:se=36'

if [ -x /usr/local/bin/vim ]; then
   export VISUAL="/usr/local/bin/vim"
else
   export VISUAL="/usr/bin/vim"
fi
export EDITOR=$VISUAL

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
if [[ $TERM != 'dumb' ]]; then
  eval `dircolors $HOME/github_repos/dircolors-solarized/dircolors.256dark`
fi
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
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._]=** r:|=**' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._]=** r:|=**'
zstyle ':completion:*' menu select=0
zstyle ':completion:*' original false
zstyle ':completion:*' prompt '%e errors:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false

autoload -Uz compinit

compinit
# End of lines added by compinstall

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function

alias vi='vim'
vim() {
   stty -ixon
   env vim $*
   stty ixany
}
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
alias bower='noglob bower'
alias node='nodejs'
alias find='noglob find'

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
  hash=`$MD5 $HOME/.zshrc`
  $VISUAL $HOME/.zshrc
  newhash=`$MD5 $HOME/.zshrc`
  if [[ $hash != $newhash ]]; then
     source $HOME/.zshrc
  fi
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
    TARGET=`egrep "^$1:" ~/.ssh_dests | cut -d: -f2`
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

dirsave() {
  pwd | ctext
}
dirgo() {
  cd `ptext`
}

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
   python -u -c "from __future__ import print_function; import sys;$1"
}

activate_env() {
   if [[ -e bin/activate ]]; then
     echo "sourcing local env: `pwd`/bin/activate"
     source bin/activate
   else
     env=$1
     pushd $HOME/python_envs/ > /dev/null

     if [[ $env == "" ]]; then
        counter=1
        typeset -A choices
        unset choice
        for x in `ls`; do
           echo $counter\) $x
           choices[$counter]=$x
           (( counter++ ))
        done
        echo -n "your choice? "
        choice=-1
        read choice
        if [[ $choice == "" ]]; then
           return
        fi
        env=$choices[$choice]
        echo "you chose $env"
     fi
     source $env/bin/activate
     popd > /dev/null
   fi
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
   if [[ -d .git ]]; then
     echo "Git log messages:"
     git log -n 5 | egrep --color=yes -Io '(TODO|NOTE|FIXME|BUG|DONE):.*$'
   fi

   echo "Messages from files:"
   egrep --color=yes -IHnro '(TODO|NOTE|FIXME|BUG):.*$' . |
   psc '
for line in sys.stdin:
      line = line.strip().split(":", 2)
      print("%s\n\t%s" % (":".join(line[2:]), ":".join(line[:2])))'
}

wiki() {
   pushd $HOME/mywiki > /dev/null
   soywiki 
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

dis() {
   jobs
   echo -n 'disown which? '
   n=-1
   read n
   if [[ $n != "" ]]; then
      disown %$n
   fi
}

getcommands() {
  compgen -acbk -A function | grep -v '^_'
}

alias dq=dmenu_queue_mpd
alias dqp=dmenu_queueplay_mpd

source $HOME/.localzshrc.sh
if [[ $BINDKEYS == "" ]]; then
  echo 'defining bindkeys in zshrc'
  BINDKEYS=${TERM%-256color}
  BINDKEYS=${BINDKEYS%-noit}
fi

bindkey -e
if [[ $BINDKEYS == "screen" ]]; then
  bindkey '[D' backward-word
  bindkey '[C' forward-word
  bindkey '[1~' beginning-of-line
  bindkey '[4~' end-of-line
else
  bindkey '[1;5D' backward-word
  bindkey '[5D' backward-word
  bindkey '[1;5C' forward-word
  bindkey '[5C' forward-word
  bindkey 'OH' beginning-of-line
  bindkey 'OF' end-of-line
fi
bindkey '[3~' delete-char

echo 'zshrc done'

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin

export CPATH=$CPATH:$HOME/include
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/lib
export VIMCLOJURE_SERVER_JAR="$HOME/lib/vimclojure/server-2.3.6.jar"

#set_colors()
#{
    #local base03="002b36"
    #local base02="073642"
    #local base01="586e75"
    #local base00="657b83"
    #local base0="839496"
    #local base1="93a1a1"
    #local base2="eee8d5"
    #local base3="fdf6e3"
    #local yellow="b58900"
    #local orange="cb4b16"
    #local red="dc322f"
    #local magenta="d33682"
    #local violet="6c71c4"
    #local blue="268bd2"
    #local cyan="2aa198"
    #local green="859900"

    #echo -en "\e]P0${base02}" #black
    #echo -en "\e]P8${base03}" #brblack
    #echo -en "\e]P1${red}" #red
    #echo -en "\e]P9${orange}" #brred
    #echo -en "\e]P2${green}" #green
    #echo -en "\e]PA${base01}" #brgreen
    #echo -en "\e]P3${yellow}" #yellow
    #echo -en "\e]PB${base00}" #bryellow
    #echo -en "\e]P4${blue}" #blue
    #echo -en "\e]PC${base0}" #brblue
    #echo -en "\e]P5${magenta}" #magenta
    #echo -en "\e]PD${violet}" #brmagenta
    #echo -en "\e]P6${cyan}" #cyan
    #echo -en "\e]PE${base1}" #brcyan
    #echo -en "\e]P7${base2}" #white
    #echo -en "\e]PF${base3}" #brwhite
    ##clear #for background artifacting
#}

#if [ "$TERM" = "linux" ]; then
    #set_colors
#fi

#unset -f set_colors

pmkdir() {
  mkdir $1
  touch $1/__init__.py
  cd $1 
}

mkcd() {
  mkdir "$1"
  cd "$1"
}

### load my plugins

for x in `ls $HOME/.zsh.d/*.zsh`; do
  source "$x"
done
alias cn=current_news

# vim: set filetype=sh:
