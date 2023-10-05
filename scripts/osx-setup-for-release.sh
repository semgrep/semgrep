#!/usr/bin/env bash
set -eux

# Setup the environment under MacOS to build and release semgrep-core.

# history: there used to be a separate osx-m1-release.sh script
# that was mostly a copy of this file, but now the
# build steps are identical so we just have one script.

# Note that this script runs from a self-hosted CI runner which
# does not reset the environment between each run, so you may
# need to do more cleanup than usually necessary.

brew install opam
opam init --no-setup --bare
#still needed?
#brew update

# we might need recent packages and the self-hosted runner opam might be behind
opam update

# Some CI runners have tree-sitter preinstalled which interfere with
# out static linking plans below so better to remove it.
# TODO: fix setup-m1-builder.sh instead?
brew uninstall --force semgrep
brew uninstall --force tree-sitter

SWITCH_NAME="${1:-5.1.0}"

#coupling: this should be the same version than in our Dockerfile
if opam switch "${SWITCH_NAME}" ; then
    # This happens because the self-hosted CI runners do not
    # cleanup things between each run.
    echo "Switch ${SWITCH_NAME} exists, continuing"
else
    echo "Switch ${SWITCH_NAME} doesn't yet exist, creating..."
    opam switch create "${SWITCH_NAME}"
    opam switch "${SWITCH_NAME}"
fi
eval "$(opam env)"

#pad:??? What was for? This was set only for the M1 build before
# Needed so we don't make config w/ sudo
export HOMEBREW_SYSTEM=1

make install-deps-MACOS-for-semgrep-core
make install-deps-for-semgrep-core

# Remove dynamically linked libraries to force MacOS to use static ones.
ls -l "$(brew --prefix)"/opt/pcre/lib || true
ls -l "$(brew --prefix)"/opt/gmp/lib || true
rm -f "$(brew --prefix)"/opt/pcre/lib/libpcre.1.dylib
rm -f "$(brew --prefix)"/opt/gmp/lib/libgmp.10.dylib

# This needs to be done after make install-deps-xxx but before make core
TREESITTER_LIBDIR=libs/ocaml-tree-sitter-core/tree-sitter/lib
echo "TREESITTER_LIBDIR is $TREESITTER_LIBDIR and contains:"
ls -l "$TREESITTER_LIBDIR" || true

echo "Deleting all the tree-sitter dynamic libraries to force static linking."
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.dylib
