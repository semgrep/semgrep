#!/usr/bin/env sh
# This script build "wheels", which is a format used by the Pypi package manager
# to distribute binaries (for us semgrep-core) with regular Python code.
# See https://packaging.python.org/en/latest/glossary/#term-Wheel
# and https://realpython.com/python-wheels/ for more information.
# This script is called from our GHA build-xxx workflows.
# It assumes the semgrep-core binary has been copied under cli/src/semgrep/bin
# for pip to package semgrep correctly.

set -e

if [ -z "$1" ]; then
    echo "missing 1st argument: wheel file"
    exit 1
fi

pip install "$1"

semgrep --version

# shellcheck disable=SC2016
echo '1 == 1' | semgrep -l python -e '$X == $X' -
