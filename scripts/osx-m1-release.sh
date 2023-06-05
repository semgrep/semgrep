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
brew install opam pkg-config coreutils pcre gmp gettext
brew update # Needed to sidestep bintray brownout

#coupling: this should be the same version than in our Dockerfile
if opam switch 4.14.0 ; then
    echo "Switch 4.14.0 exists, continuing"
else
    echo "Switch 4.14.0 doesn't yet exist, creating..."
    opam switch create 4.14.0
    opam switch 4.14.0
fi
git submodule update --init --recursive --depth 1

eval "$(opam env)"

# Needed so we don't make config w/ sudo
export HOMEBREW_SYSTEM=1

# Remove libraries dynamically linked to force MacOS to use static.
# This needs to be done before make setup since it is used there.
ls -l "$(brew --prefix)"/opt/pcre/lib || true
ls -l "$(brew --prefix)"/opt/gmp/lib || true
rm -f "$(brew --prefix)"/opt/pcre/lib/libpcre.1.dylib
rm -f "$(brew --prefix)"/opt/gmp/lib/libgmp.10.dylib

make setup

# Remove dynamically linked libraries to force MacOS to use static ones
# This needs to be done after make setup but before make build-*
TREESITTER_LIBDIR=libs/ocaml-tree-sitter-core/tree-sitter/lib
echo "TREESITTER_LIBDIR is $TREESITTER_LIBDIR and contains:"
ls -l "$TREESITTER_LIBDIR" || true

echo "Deleting all the tree-sitter dynamic libraries to force static linking."
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.dylib

make core
make core-install

mkdir -p artifacts
cp ./_build/install/default/bin/semgrep-core artifacts
zip -r artifacts.zip artifacts
