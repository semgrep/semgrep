#!/bin/sh
set -e

THIS_DIR="$(dirname "$(realpath "$0")")";
cd "${THIS_DIR}"

pip3 install Nuitka==0.6.7
pip3 install -r requirements.txt

echo "building and placing output binary in ${1}"
python3.7 -m nuitka --follow-imports --standalone --show-modules --recurse-to=colorama --output-dir="${1}" --warn-unusual-code --lto ./semgrep/__main__.py

# rename to sgrep-lint
cp -v ./sgrep-lint "${1}__main__.dist/sgrep-lint"
# Temporary hack so it's where old build scripts expect it
mv "${1}__main__.dist" "${1}sgrep.dist"
