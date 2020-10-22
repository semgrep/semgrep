#!/bin/bash
set -e
brew install opam pkg-config coreutils
opam init --no-setup --bare;
opam switch create 4.10.0;
opam switch 4.10.0;
git submodule update --init --recursive

eval "$(opam env)"

make setup
make config

# Remove dynamically linked libraries to force MacOS to use static ones
rm /usr/local/lib/libtree-sitter.0.0.dylib
rm /usr/local/lib/libtree-sitter.dylib

make build-spacegrep
make build-core

mkdir -p artifacts
cp ./semgrep-core/_build/install/default/bin/semgrep-core artifacts
cp ./spacegrep/_build/install/default/bin/spacegrep artifacts
zip -r artifacts.zip artifacts
