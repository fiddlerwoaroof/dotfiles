#zmodload zsh/zprof

cat <<'EOP'
             :
    `.       ;        .'
      `.  .-'''-.   .'
        ;'  __   _;'
       /   '_    _`\
      |  _( a (  a  |
 '''''| (_)    >    |``````
       \    \    / /
        `.   `--'.'
       .' `-,,,-' `.
     .'      :      `.  hjw
             :
EOP

echo "begin zshrc"
echo "shell session started for $USER on $TTY at `date`" | logger
source $HOME/.localzshrc.sh
autoload -U colors && colors
autoload zsh/parameter

# Experimenting with disabling oh-my-zsh
#
# # Path to your oh-my-zsh configuration.
# #export ZSH=$HOME/.oh-my-zsh
# #export MPD_HOST=srv2.elangley.org
# #
# #fpath=(~/.zsh.d/completion ~/.zsh.d/functions $fpath)
# #plugins=(git rails osx brew zsh-syntax-highlighting python git-extra git-flow battery)
# #
# #source $ZSH/oh-my-zsh.sh
# #unsetopt correct_all
# #
# #echo "done oh-my-zsh"
# unalias sp

export PATH=/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/usr/texbin:/usr/X11/bin:/opt/local/bin:/sbin/usr/sbin:$PATH

if [[ -e /etc/sysconfig/zsh-prompt-$TERM ]]; then
  . /etc/sysconfig/zsh-prompt-$TERM
elif [[ -e $HOME/.zsh-prompt-$TERM ]]; then
  . $HOME/.zsh-prompt-$TERM
fi

for p in $PATH; do
  _FORTUNE="$PATH/fortune"
  if [[ -x "$_FORTUNE" ]]; then
    FORTUNE="$_FORTUNE"
    break
  fi
done

if [ -x "$FORTUNE" ]; then
  $FORTUNE
fi

function battery_charge() {
  # the -S is for performance
  python -S "$HOME/bin/batcharge.py" 2>/dev/null
}

autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats \
    '%F{5}%f%s%F{5}%F{3}->%F{5}%F{2}%b%F{3}|%F{1}%a%F{5}%f'
zstyle ':vcs_info:*' formats       \
  '%F{5}%f%s%F{5}%F{3}->%F{5}%F{2}%b%F{5}%f'
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'

zstyle ':vcs_info:*' enable git cvs svn

vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}

export PYTHONSTARTUP=$HOME/Library/Python/2.7/site-packages/sitecustomize.py
setopt promptsubst
PROMPT='---
(%?) %m:%n--%l ${PWD/$HOME/~} `vcs_info_wrapper` `battery_charge`
%!:%# '
export PROMPT

HOSTNAME=`hostname -f`
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'

cmdtermtitle() {
  cmd_name="${(V)1}"
  if [ 'fg' = "${${(z)@}[1]}" ]; then
    cmd_name="${(vV)jobtexts}"
  fi

  if [[ "${TERM%%-*}"x == "screen"x ]]; then
    echo -ne "\033]0;${cmd_name}\007"
  else
    echo -ne "\033]0; ${cmd_name} : ${USER}@$HOSTNAME\007"
  fi
}

if [[ $TERM != "linux" ]]; then
   autoload -U add-zsh-hook
   add-zsh-hook preexec cmdtermtitle
fi

termtitle() {
  npwd=${PWD/#$HOME/\~}
  if [[ "${TERM%%-*}"x == "screen"x ]]; then
    echo -ne "\033]0;${npwd}\007"
  else
    echo -ne "\033]0;${USER}@$HOSTNAME: ${npwd}\007"
  fi
}

if [[ $TERM != "linux" ]]; then
   add-zsh-hook precmd termtitle
fi

PATH=/home/edwlan/bin:/usr/local/bin:$PATH
export PATH="/opt/local/bin:/usr/sbin:/sbin/usr/sbin:/sbin:$HOME/.cabal/bin:$HOME/.local/bin:$HOME/bin:/Developer/usr/bin:$PATH"
export VIMCLOJURE_SERVER_JAR="$HOME/bin/jars/server-2.3.6.jar"
export INFOPATH=/usr/local/share/info:/usr/local/texlive/2009/texmf/doc/info
export SAVEHIST=10000000
export HISTSIZE=10000000
export HISTFILE=$HOME/.zshistory

export PKG_CONFIG_PATH="$PKG_CONFIG_PATH"

export MANPATH="/opt/local/share/man:/Applications/Xcode.app/Contents/Developer/usr/share/man:$MANPATH"

export PAGER="less -SiemX"

export RGBDEF='/opt/X11/share/X11/rgb.txt'
export GREP_COLORS='ms=01;31:mc=01;31:sl=:cx=:fn=35:ln=32:bn=32:se=36'

if [ -x /usr/local/bin/vim ]; then
   export VISUAL="/usr/local/bin/vim"
else
   export VISUAL="/usr/bin/vim"
fi
export EDITOR=$VISUAL

autoload run-help
autoload -U zfinit
autoload -U tcp_proxy
autoload -U tcp_open
autoload -U tcp_point
autoload -U tcp_shoot
#------------------
zfinit

if [[ $TERM != 'dumb' ]]; then
  # Solarized dircolors:
  LS_COLORS='no=00;38;5;244:rs=0:di=00;38;5;33:ln=00;38;5;37:mh=00:pi=48;5;230;38;5;136;01:so=48;5;230;38;5;136;01:do=48;5;230;38;5;136;01:bd=48;5;230;38;5;244;01:cd=48;5;230;38;5;244;01:or=48;5;235;38;5;160:su=48;5;160;38;5;230:sg=48;5;136;38;5;230:ca=30;41:tw=48;5;64;38;5;230:ow=48;5;235;38;5;33:st=48;5;33;38;5;230:ex=00;38;5;64:*.tar=00;38;5;61:*.tgz=00;38;5;61:*.arj=00;38;5;61:*.taz=00;38;5;61:*.lzh=00;38;5;61:*.lzma=00;38;5;61:*.tlz=00;38;5;61:*.txz=00;38;5;61:*.zip=00;38;5;61:*.z=00;38;5;61:*.Z=00;38;5;61:*.dz=00;38;5;61:*.gz=00;38;5;61:*.lz=00;38;5;61:*.xz=00;38;5;61:*.bz2=00;38;5;61:*.bz=00;38;5;61:*.tbz=00;38;5;61:*.tbz2=00;38;5;61:*.tz=00;38;5;61:*.deb=00;38;5;61:*.rpm=00;38;5;61:*.jar=00;38;5;61:*.rar=00;38;5;61:*.ace=00;38;5;61:*.zoo=00;38;5;61:*.cpio=00;38;5;61:*.7z=00;38;5;61:*.rz=00;38;5;61:*.apk=00;38;5;61:*.gem=00;38;5;61:*.jpg=00;38;5;136:*.JPG=00;38;5;136:*.jpeg=00;38;5;136:*.gif=00;38;5;136:*.bmp=00;38;5;136:*.pbm=00;38;5;136:*.pgm=00;38;5;136:*.ppm=00;38;5;136:*.tga=00;38;5;136:*.xbm=00;38;5;136:*.xpm=00;38;5;136:*.tif=00;38;5;136:*.tiff=00;38;5;136:*.png=00;38;5;136:*.PNG=00;38;5;136:*.svg=00;38;5;136:*.svgz=00;38;5;136:*.mng=00;38;5;136:*.pcx=00;38;5;136:*.dl=00;38;5;136:*.xcf=00;38;5;136:*.xwd=00;38;5;136:*.yuv=00;38;5;136:*.cgm=00;38;5;136:*.emf=00;38;5;136:*.eps=00;38;5;136:*.CR2=00;38;5;136:*.ico=00;38;5;136:*.tex=00;38;5;245:*.rdf=00;38;5;245:*.owl=00;38;5;245:*.n3=00;38;5;245:*.ttl=00;38;5;245:*.nt=00;38;5;245:*.torrent=00;38;5;245:*.xml=00;38;5;245:*Makefile=00;38;5;245:*Rakefile=00;38;5;245:*Dockerfile=00;38;5;245:*build.xml=00;38;5;245:*rc=00;38;5;245:*1=00;38;5;245:*.nfo=00;38;5;245:*README=00;38;5;245:*README.txt=00;38;5;245:*readme.txt=00;38;5;245:*.md=00;38;5;245:*README.markdown=00;38;5;245:*.ini=00;38;5;245:*.yml=00;38;5;245:*.cfg=00;38;5;245:*.conf=00;38;5;245:*.c=00;38;5;245:*.cpp=00;38;5;245:*.cc=00;38;5;245:*.sqlite=00;38;5;245:*.go=00;38;5;245:*.log=00;38;5;240:*.bak=00;38;5;240:*.aux=00;38;5;240:*.lof=00;38;5;240:*.lol=00;38;5;240:*.lot=00;38;5;240:*.out=00;38;5;240:*.toc=00;38;5;240:*.bbl=00;38;5;240:*.blg=00;38;5;240:*~=00;38;5;240:*#=00;38;5;240:*.part=00;38;5;240:*.incomplete=00;38;5;240:*.swp=00;38;5;240:*.tmp=00;38;5;240:*.temp=00;38;5;240:*.o=00;38;5;240:*.pyc=00;38;5;240:*.class=00;38;5;240:*.cache=00;38;5;240:*.aac=00;38;5;166:*.au=00;38;5;166:*.flac=00;38;5;166:*.mid=00;38;5;166:*.midi=00;38;5;166:*.mka=00;38;5;166:*.mp3=00;38;5;166:*.mpc=00;38;5;166:*.ogg=00;38;5;166:*.ra=00;38;5;166:*.wav=00;38;5;166:*.m4a=00;38;5;166:*.axa=00;38;5;166:*.oga=00;38;5;166:*.spx=00;38;5;166:*.xspf=00;38;5;166:*.mov=00;38;5;166:*.MOV=00;38;5;166:*.mpg=00;38;5;166:*.mpeg=00;38;5;166:*.m2v=00;38;5;166:*.mkv=00;38;5;166:*.ogm=00;38;5;166:*.mp4=00;38;5;166:*.m4v=00;38;5;166:*.mp4v=00;38;5;166:*.vob=00;38;5;166:*.qt=00;38;5;166:*.nuv=00;38;5;166:*.wmv=00;38;5;166:*.asf=00;38;5;166:*.rm=00;38;5;166:*.rmvb=00;38;5;166:*.flc=00;38;5;166:*.avi=00;38;5;166:*.fli=00;38;5;166:*.flv=00;38;5;166:*.gl=00;38;5;166:*.m2ts=00;38;5;166:*.divx=00;38;5;166:*.webm=00;38;5;166:*.axv=00;38;5;166:*.anx=00;38;5;166:*.ogv=00;38;5;166:*.ogx=00;38;5;166:';
  export LS_COLORS
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
alias jmp=pushd
alias ret=popd
alias ..python="PYTHONPATH=.. python"
alias .python="PYTHONPATH=. python"
alias grep="grep --color=auto -I"
alias la="ls -AF"
alias ,=pydit
alias tw=twitter_tool
alias v=$VISUAL
alias e=$EDITOR
alias cvsdiff='cvs diff -wbB | colordiff'
alias cp.='gcp --target-directory=.'
alias bower='noglob bower'
alias node='nodejs'
alias find='noglob find'

echo "done variables and options"

showspaces() {
	python -c'import sys;print sys.stdin.read().replace(" ","_").replace("\t", "----")'
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
  venv=.
  if [[ -e bin/activate ]]; then
    echo "sourcing local env: `pwd`/bin/activate"
  elif [[ -e venv/bin/activate ]]; then
    echo "sourcing local env: `pwd`/venv/bin/activate"
    venv=venv
  else
    env=$1
    pushd $HOME/python_envs/ > /dev/null
    venv="$PWD/$env"
    popd

    if [[ $env == "" ]]; then
      counter=1
      typeset -A choices
      unset choice
      for x in `ls "$venv"`; do
        echo $counter\) `basename $x`
        choices[$counter]=$x
        (( counter++ ))
      done
      echo -n "your choice? "
      choice=-1
      read choice
      if [[ $choice == "" ]]; then
        return
      fi
      venv="$venv/$choices[$choice]"
      echo "you chose $venv"
    fi
  fi
  source $venv/bin/activate
  unset venv env;
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
   find \( \( -name .git -o -name CVS \) -prune \) -o  \( -type f \) -print0  | xargs -0 shasum | sort | uniq -w 20 -c | sort -nr
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

pmkdir() {
  mkdir $1
  touch $1/__init__.py
  cd $1
}

mkcd() {
  mkdir "$1"
  cd "$1"
}

groot() {
  cd `git rev-parse --show-toplevel`
}

for x in `ls $HOME/.zsh.d/*.zsh`; do
  source "$x"
done
alias cn=current_news

#THIS MUST BE AT THE END OF THE FILE FOR GVM TO WORK!!!
[[ -s "/Users/edwlan/.gvm/bin/gvm-init.sh" ]] && source "/Users/edwlan/.gvm/bin/gvm-init.sh"

#zprof
