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

# Some CI runners have tree-sitter preinstalled which interfere with
# out static linking plans below so better to remove it.
# TODO: fix setup-m1-builder.sh instead?
brew uninstall --force semgrep
brew uninstall --force tree-sitter

SWITCH_NAME="${1:-4.14.0}"

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
# We do this so we build LWT with libev on the path
# Coupling: This should be similar to homebrew setup
# austin: Why can't we use make homebrew-setup here? It doesn't seem to work
#         because of something with how tree-sitter is installed.
LIBRARY_PATH="$(brew --prefix)/lib" make install-deps-for-semgrep-core

# Allow pkg-config to pick up tree-sitter. Perhaps export this in the tree
# sitter install script? But not persistent. Hmm.
export PKG_CONFIG_PATH="libs/ocaml-tree-sitter-core/tree-sitter/lib/pkgconfig:$PKG_CONFIG_PATH"
