#!/usr/bin/env sh
# This script installs a semgrep python wheel and then validates that semgrep works.

set -e

if [ -z "$1" ]; then
    echo "missing 1st argument: wheel file"
    exit 1
fi

pip install "$1"

semgrep --version

# shellcheck disable=SC2016
echo '1 == 1' | semgrep -l python -e '$X == $X' -
