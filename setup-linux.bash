#!/usr/bin/env bash
set -x -e -u -o pipefail

mkdir "$HOME/emacs-home/"
cd "$HOME/emacs-home/"
git init

cd "$HOME"
mkdir "$HOME/git_repos"
git clone https://git.fiddlerwoaroof.com/git/dotfiles.git "$HOME/git_repos/dotfiles"
mkdir .emacs.d
ln -s "$HOME/git_repos/dotfiles/emacs.d/init.el" "$HOME/.emacs.d/init.el"
mkdir "$HOME/.emacs.d/"{lisp,themes}
ln -s "$HOME/git_repos/dotfiles/emacs.d/lisp/"*.el "$HOME/.emacs.d/lisp"
rm "$HOME/git_repos/dotfiles/emacs.d/lisp/el-zenburn-theme.el"
ln -s "$HOME/git_repos/dotfiles/emacs.d/lisp/fwoar-zenburn-theme.el" "$HOME/.emacs.d/themes"
mkdir "$HOME/.emacs.d/lisp/configurations"
ln -s "$HOME/git_repos/dotfiles/emacs.d/lisp/configurations/"*-conf.el  "$HOME/.emacs.d/lisp/configurations"
eval "$(curl -L "https://iterm2.com/shell_integration/install_shell_integration_and_utilities.sh")"
cd "$HOME/git_repos/dotfiles"
git reset --hard
