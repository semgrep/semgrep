#!/usr/bin/env bash
set -eux

# Setup the environment under MacOS to build and release semgrep-core.

# history: there used to be a separate osx-m1-release.sh script
# that was mostly a copy of this file, but now the
# build steps are identical so we just have one script.
# Was previously combined with osx-setup-opam-for-release.sh

#pad:??? What was for? This was set only for the M1 build before
# Needed so we don't make config w/ sudo
export HOMEBREW_SYSTEM=1

make install-deps-MACOS-for-semgrep-core
# We do this so we build LWT with libev on the path
# Coupling: This should be similar to homebrew setup
# austin: Why can't we use make homebrew-setup here? It doesn't seem to work
#         because of something with how tree-sitter is installed.
LIBRARY_PATH="$(brew --prefix)/lib" make install-deps-for-semgrep-core

# Remove dynamically linked libraries to force MacOS to use static ones.
ls -l "$(brew --prefix)"/opt/pcre/lib || true
ls -l "$(brew --prefix)"/opt/pcre2/lib || true
ls -l "$(brew --prefix)"/opt/gmp/lib || true
ls -l "$(brew --prefix)"/opt/libev/lib || true
rm -f "$(brew --prefix)"/opt/pcre/lib/libpcre.1.dylib
rm -f "$(brew --prefix)"/opt/pcre2/lib/libpcre2-8.0.dylib
rm -f "$(brew --prefix)"/opt/gmp/lib/libgmp.10.dylib
rm -f "$(brew --prefix)"/opt/libev/lib/libev.4.dylib

# This needs to be done after make install-deps-xxx but before make core
TREESITTER_LIBDIR=libs/ocaml-tree-sitter-core/tree-sitter/lib
echo "TREESITTER_LIBDIR is $TREESITTER_LIBDIR and contains:"
ls -l "$TREESITTER_LIBDIR" || true

echo "Deleting all the tree-sitter dynamic libraries to force static linking."
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.0.dylib
rm -f "$TREESITTER_LIBDIR"/libtree-sitter.dylib
