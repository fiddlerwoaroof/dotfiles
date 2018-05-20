savedir() {
    cmd="hash -d \"$1\"=\"$PWD/${(j</>)*}\""
    eval $cmd
    truncfile 50 $HOME/.gtaliases
    echo $cmd >> $HOME/.gtaliases
}

hash -d "desktop=$HOME/Desktop"
hash -d "downloads=$HOME/Downloads"
hash -d "programming=$HOME/Programming"

git_dir=($HOME/git*_repos)
num_git_dirs="$git_dir[(I)$git_dir[-1]]" 
if (( num_git_dirs > 0 )); then
  hash -d "g=${git_dir[1]}"
else
  echo no git directories
fi

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
