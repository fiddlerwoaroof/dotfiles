all: git-pick-patch

bindir:
	mkdir -p bin

git-pick-patch: bindir git-pick-patch.lisp
	sbcl --eval "(ql:quickload '(:alexandria :serapeum :cl-ppcre))" \
       --load git-pick-patch.lisp \
       --eval '(save-lisp-and-die "bin/git-pick-patch"'" :executable t :toplevel 'git-pick-patch::main :compression t)"

install: all
	mkdir -p ${HOME}/bin
	ln -s bin/* ${HOME}/bin
