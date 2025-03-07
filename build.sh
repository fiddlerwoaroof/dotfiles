#!/usr/bin/env zsh

# set -x
# set -eu -o pipefail

source "$stdenv"/setup

export HOME=$(mktemp -d)
buildPhase() {
  cd "$HOME"
  cp -R "$src" src
  chmod -R +w src
  cd src
  echo -n "NOTICE ME: "
  pwd
  ls -dl "$PWD"
  sbcl --lose-on-corruption --disable-ldb --disable-debugger \
       --no-userinit --no-sysinit \
       --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
       --eval '(push :fw.main *features*)' \
       --eval '(require :asdf)' \
       --eval '(asdf:load-asd (truename "tools.asd"))' \
       --eval '(asdf:load-system :tools/zenburn)' \
       --load zenburn.lisp \
       --eval "(fwoar.zenburn:dump)"
}

installPhase() {
  mkdir -p "$out"/bin
  mv zenburn "$out"/bin
  mkdir -p "$out"/lib/sbcl/
  #cp "$(dirname "$(which sbcl)")"/../lib/sbcl/sbcl.core "$out"/lib/sbcl/
  env
  wrapProgram "$out/bin/zenburn"
}

genericBuild
