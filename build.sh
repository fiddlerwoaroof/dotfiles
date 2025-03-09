#!/usr/bin/env zsh

# set -x
set -e -o pipefail

source "$stdenv"/setup

export HOME=$(mktemp -d)
buildPhase() {
  cd "$HOME"
  cp -R "$src" src
  chmod -R +w src
  cd src
  sbcl --lose-on-corruption --disable-ldb --disable-debugger \
       --no-userinit --no-sysinit \
       --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
       --eval '(push :fw.main *features*)' \
       --eval '(require :asdf)' \
       --eval '(asdf:load-asd (truename "tools.asd"))' \
       --eval "(asdf:load-system :tools/$name)" \
       --eval "(asdf:operate :program-op :tools/$name)"
}

installPhase() (
  set -x
  mkdir -p "$out"/bin
  mv "$HOME/$name" "$out"/bin
  wrapProgram "$out/bin/$name"
)

genericBuild
