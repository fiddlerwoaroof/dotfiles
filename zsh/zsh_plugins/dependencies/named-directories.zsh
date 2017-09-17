savedir() {
    cmd="hash -d \"$1\"=\"$PWD/${(j</>)*}\""
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
