#!/usr/bin/env sh
# This script installs a semgrep python wheel and then validates that semgrep works.

set -e

if [ -z "$1" ]; then
    echo "missing 1st argument: wheel file"
    exit 1
fi

# temporary workaround for https://github.com/semgrep/semgrep/pull/8336/commits/565ba8443bd1ca4f2cd18fabb6ee61af971c5dda,
# which forces amd64 builds to use the `any` platform compatibility tag and
# causes the glob that we're calling this script with not match anything
# this will be fixed in the near future as we factor python wheel building out
# of the main Dockerfile and update amd64 to use the same approach as arm64
# (see: https://github.com/semgrep/semgrep/pull/8371)
if [ ! -f "$1" ]; then
    echo "not a file: $1"
    exit 0
fi

pip install "$1"

semgrep --version

# shellcheck disable=SC2016
echo '1 == 1' | semgrep -l python -e '$X == $X' -
