#!/usr/bin/env bash

# Because we're running this on a remote machine, we don't want to reinstall
# everything every time
set -e
brew update # Needed to sidestep bintray brownout
#coupling: this should be the same version than in our Dockerfile
opam switch 4.12.0;
git submodule update --init --recursive --depth 1

eval "$(opam env)"

# Needed so we don't make config w/ sudo
export HOMEBREW_SYSTEM=1

make setup
make config

make build-core

mkdir -p artifacts
cp ./semgrep-core/_build/install/default/bin/semgrep-core artifacts
zip -r artifacts.zip artifacts
