#!/usr/bin/env bash

# Because we're running this on a remote machine, we don't want to reinstall
# everything every time
set -e
eval "$(opam env)"

# Needed so we don't make config w/ sudo
export HOMEBREW_SYSTEM=1

make setup
make config

# Remove dynamically linked libraries to force MacOS to use static ones
# This needs to be done after make setup but before make build-*
rm /usr/local/lib/libtree-sitter.0.0.dylib || true
rm /usr/local/lib/libtree-sitter.dylib || true

make build-core

mkdir -p artifacts
cp ./semgrep-core/_build/install/default/bin/semgrep-core artifacts
zip -r artifacts.zip artifacts
