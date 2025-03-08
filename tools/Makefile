all: bin bin/git-pick-patch bin/bloomutil bin/zenburn bin/file-indexer bin/cls

bin/zenburn: zenburn.lisp
	sbcl --lose-on-corruption --disable-ldb --disable-debugger \
		  --no-userinit --no-sysinit \
	    --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
		  --eval '(push :fw.dump *features*)' \
		  --load zenburn.lisp \
		  --eval '(fwoar.zenburn:dump)'
	mv zenburn bin

bin:
	mkdir -p bin

bin/bloomutil: bloomutil.lisp
	sbcl --disable-debugger \
	     --no-userinit \
	     --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
	     --eval '(pushnew :fw.dump *features*)' \
	     --load bloomutil.lisp \
	     --eval '(fwoar.bloomutil::dump)'
	mv bloomutil bin

bin/file-indexer: file-indexer.lisp
	sbcl --disable-debugger \
	     --no-userinit \
	     --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
	     --eval '(pushnew :fw.dump *features*)' \
	     --load file-indexer.lisp \
	     --eval '(fwoar.file-indexer::dump)'
	mv file-indexer bin

bin/git-pick-patch: git-pick-patch.lisp
	sbcl --disable-ldb --lose-on-corruption --disable-debugger --no-userinit --no-sysinit \
	     --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
       --load "$(HOME)"/quicklisp/setup.lisp \
	     --eval "(ql:quickload '(:alexandria :serapeum :cl-ppcre))" \
	     --load git-pick-patch.lisp \
	     --eval '(save-lisp-and-die "bin/git-pick-patch"'" :executable t :toplevel 'git-pick-patch::main :compression t)"

bin/cls: cls.lisp
	sbcl --disable-debugger \
	     --no-userinit \
	     --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
	     --eval '(pushnew :fw.dump *features*)' \
	     --load cls.lisp \
	     --eval '(fwoar.cls::dump)'
	mv cls bin

bin/json-formatter: json-formatter.lisp
	sbcl --disable-debugger \
	     --no-userinit \
	     --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
	     --eval '(pushnew :fw.dump *features*)' \
	     --load json-formatter.lisp \
	     --eval '(fwoar.json-formatter::dump)'
	mv json-formatter bin

install: all
	mkdir -p ${HOME}/bin
	ln -sf ${PWD}/bin/* ${HOME}/bin
