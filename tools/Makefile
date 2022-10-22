all: git-pick-patch bloomutil

bindir:
	mkdir -p bin

bloomutil: bloomutil.lisp
	sbcl --disable-debugger \
	     --no-userinit \
	     --eval '(pushnew :fw.dump *features*)' \
	     --load bloomutil.lisp \
	     --eval '(fwoar.bloomutil::dump)'
	mv bloomutil bin

git-pick-patch: bindir git-pick-patch.lisp
	sbcl --eval "(ql:quickload '(:alexandria :serapeum :cl-ppcre))" \
	     --load git-pick-patch.lisp \
	     --eval '(save-lisp-and-die "bin/git-pick-patch"'" :executable t :toplevel 'git-pick-patch::main :compression t)"

install: all
	mkdir -p ${HOME}/bin
	ln -sf ${PWD}/bin/* ${HOME}/bin
