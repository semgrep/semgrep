#!/usr/bin/env bash
set -eux

# Build semgrep-core for MacOS

# history: there used to be a separate osx-m1-release.sh script
# that was mostly a copy of osx-release.sh, but now the
# build steps are identical so we just have one script.

# Note that this script runs from a self-hosted CI runner which
# does not reset the environment between each run, so you may
# need to do more cleanup than usually necessary.

brew install opam pkg-config coreutils pcre gettext

#still needed?
#brew update
#opam init --no-setup --bare

#coupling: this should be the same version than in our Dockerfile
if opam switch 4.14.0 ; then
    # This happens because the self-hosted CI runners do not
    # cleanup things between each run.
    echo "Switch 4.14.0 exists, continuing"
else
    echo "Switch 4.14.0 doesn't yet exist, creating..."
    opam switch create 4.14.0
    opam switch 4.14.0
fi

git submodule update --init --recursive --depth 1

eval "$(opam env)"

#pad:??? What was for? This was set only for the M1 build before
# Needed so we don't make config w/ sudo
export HOMEBREW_SYSTEM=1

# Remove pcre dynamically linked to force MacOS to use static.
# This needs to be done before make setup since it is used there.
ls -l "$(brew --prefix)"/opt/pcre/lib || true
rm -f "$(brew --prefix)"/opt/pcre/lib/libpcre.1.dylib

make setup

# Remove dynamically linked libraries to force MacOS to use static ones.
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
