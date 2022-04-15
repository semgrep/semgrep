#! /usr/bin/env bash
set -e
brew update # Needed to sidestep bintray brownout
brew install opam pkg-config coreutils
opam init --no-setup --bare;
#coupling: this should be the same version than in our Dockerfile
opam switch create 4.12.0;
opam switch 4.12.0;
git submodule update --init --recursive --depth 1

eval "$(opam env)"

# Remove pcre dynamically linked to force MacOS to use static
# This needs to be done before make setup since it is used there
rm /usr/local/opt/pcre/lib/libpcre.1.dylib

make setup
make config

# Remove dynamically linked libraries to force MacOS to use static ones
# This needs to be done after make setup but before make build-*
rm /usr/local/lib/libtree-sitter.0.0.dylib
rm /usr/local/lib/libtree-sitter.dylib

make build-core

mkdir -p artifacts
cp ./semgrep-core/_build/install/default/bin/semgrep-core artifacts
zip -r artifacts.zip artifacts
