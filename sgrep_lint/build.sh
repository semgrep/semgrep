#!/bin/sh
set -e

THIS_DIR="$(dirname "$(realpath "$0")")";
cd "${THIS_DIR}"

git clone -b develop https://github.com/Nuitka/Nuitka
cd Nuitka
python3.7 setup.py install
cd ..

pip3 install -r requirements.txt

echo "building and placing output binary in ${1}"
python3.7 -m nuitka --follow-imports --standalone --show-modules --recurse-to=colorama --output-dir="${1}" --warn-unusual-code --lto ./sgrep.py

# rename to sgrep-lint
mv "${1}/sgrep.dist/sgrep" "${1}/sgrep.dist/sgrep-lint-exe"
cp -v ./sgrep-lint "${1}/sgrep.dist/sgrep-lint"
