#!/usr/bin/env bash
# Build semgrep-core for MacOS
#
# This script is a quasi-duplicate of osx-release.sh. It looks like maybe
# these files used to be different but now are identical.
# TODO: share code with osx-release.sh. Having a command-line option for m1
# would be preferable over maintaining duplicate code.
#
set -eux

# Because we're running this on a remote machine, we don't want to reinstall
# everything every time

brew update # Needed to sidestep bintray brownout
brew install opam pkg-config coreutils pcre
opam init --no-setup --bare;
#coupling: this should be the same version than in our Dockerfile
opam switch create 4.14.0
opam switch 4.14.0
git submodule update --init --recursive --depth 1

eval "$(opam env)"

make setup

# Remove pcre dynamically linked to force MacOS to use static
# This needs to be done before make setup since it is used there
ls -l /usr/local/opt/pcre/lib || true
rm -f /usr/local/opt/pcre/lib/libpcre.1.dylib

# Remove dynamically linked libraries to force MacOS to use static ones
# This needs to be done after make setup but before make build-*
TREESITTER_LIBDIR=semgrep-core/src/ocaml-tree-sitter-core/tree-sitter/lib
echo "TREESITTER_LIBDIR is $TREESITTER_LIBDIR and contains:"
ls -l "$TREESITTER_LIBDIR" || true

echo "Deleting all the tree-sitter dynamic libraries to force static linking."
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.dylib

make build-core

mkdir -p artifacts
cp ./semgrep-core/_build/install/default/bin/semgrep-core artifacts
zip -r artifacts.zip artifacts
