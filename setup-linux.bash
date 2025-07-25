#!/usr/bin/env bash
set -x -e -u -o pipefail


mkdir -p "$HOME/emacs-home/"
cd "$HOME/emacs-home/"
git init

cd "$HOME"
mkdir -p "$HOME/git_repos"

mkdir -p "$HOME"/git_repos/dotfiles
(
	cd "$HOME"/git_repos/dotfiles
if ! [[ -d .git ]]; then
	git clone https://git.fiddlerwoaroof.com/git/dotfiles.git "$HOME/git_repos/dotfiles"
else
		echo 'NOTICE ME: dotfiles already cloned'
		git status
fi
)

mkdir -p .emacs.d
ln -sfv "$HOME/git_repos/dotfiles/emacs.d/init.el" "$HOME/.emacs.d/init.el"
mkdir -p "$HOME/.emacs.d/"{lisp,themes}

if [[ -z "${git_mode:-}" ]]; then
  select git_mode in http ssh; do
    echo "(setq fwoar-git-mode :$git_mode)" > "$HOME/.emacs.d/lisp/site-lisp.el"
    echo "GIT_SSH_MODE=\"$git_mode\"" > "$HOME/.localzshrc.sh"
    break;
  done
else
    echo "(setq fwoar-git-mode :$git_mode)" > "$HOME/.emacs.d/lisp/site-lisp.el"
    echo "GIT_SSH_MODE=\"$git_mode\"" > "$HOME/.localzshrc.sh"
fi

ln -sfv "$HOME/git_repos/dotfiles/emacs.d/lisp/"*.el "$HOME/.emacs.d/lisp"
rm "$HOME/git_repos/dotfiles/emacs.d/lisp/el-zenburn-theme.el"
ln -sfv "$HOME/git_repos/dotfiles/emacs.d/lisp/fwoar-zenburn-theme.el" "$HOME/.emacs.d/themes"
mkdir -p "$HOME/.emacs.d/lisp/configurations"
ln -sfv "$HOME/git_repos/dotfiles/emacs.d/lisp/configurations/"*-conf.el  "$HOME/.emacs.d/lisp/configurations"
ln -sfv "$HOME/git_repos/dotfiles/emacs.d/emacs.d/early-init.el" ~/.emacs.d
#eval "$(curl -L "https://iterm2.com/shell_integration/install_shell_integration_and_utilities.sh")"
cd "$HOME/git_repos/dotfiles"
#git reset --hard

(cd /tmp; curl -O https://beta.quicklisp.org/quicklisp.lisp; sbcl --load quicklisp.lisp)

if [[ "$(uname -s)" == "Darwin"]]; then
	echo 'install iTerm2 https://iterm2.com and hit enter'
	read
	echo 'TODO: configure iterm2 settings repository'

	echo 'install Karabiner Elements https://karabiner-elements.pqrs.org and hit enter'
	read
	mkdir -p ~/.config
	ln -sfv "$PWD"/karabiner ~/.config

	echo 'install Moom https://manytricks.com/moom/ and hit enter'
	read
	open ./moom-settings/basics.moom
fi
