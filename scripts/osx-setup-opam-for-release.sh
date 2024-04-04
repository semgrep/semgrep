#!/usr/bin/env bash
set -eux

# Setup the opam environment under MacOS to build and release semgrep-core.
# All other MacOS-related setup should be in `osx-setup-post-opam-for-release.sh`

# We separate out opam configuration because we have some scripts which rely
# on building with opam before running the rest of MacOS setup. In particular,
# to link pcre2, we need to install opam, build with it dynamically linked,
# then remove it so it can be statically linked.

brew install opam
opam init --no-setup --bare
#still needed?
#brew update

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
