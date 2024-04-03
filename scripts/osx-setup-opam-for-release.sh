#!/usr/bin/env bash
set -eux

# Setup the environment under MacOS to build and release semgrep-core.

# history: there used to be a separate osx-m1-release.sh script
# that was mostly a copy of this file, but now the
# build steps are identical so we just have one script.
#
# Previously this was a combined file with other code to handle forcing static
# linkage, but this caused issues due to git's runtime dependency on pcre2.
# Ideally we would have a better way of causing static linking than removing
# potential runtime deps of other things in the build script.

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
