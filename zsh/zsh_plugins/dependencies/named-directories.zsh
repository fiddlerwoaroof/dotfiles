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

gtdo() { cd ~downloads/"${(j</>)*}" }

if [[ -d $HOME/dotfiles ]]; then
  hash -d zsh_conf=$HOME/dotfiles/zsh
  hash -d dotfiles=$HOME/dotfiles
elif [[ -d $HOME/git_repos ]]; then
  hash -d zsh_conf=$HOME/git_repos/dotfiles/zsh
  hash -d dotfiles=$HOME/git_repos/dotfiles
elif [[ -d $HOME/github_repos ]]; then
  hash -d zsh_conf=$HOME/github_repos/dotfiles/zsh
  hash -d dotfiles=$HOME/github_repos/dotfiles
fi
