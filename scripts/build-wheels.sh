#!/usr/bin/env bash
# This script build "wheels", which is a format used by the Pypi package manager
# to distribute binaries (for us semgrep-core) with regular Python code.
# See https://packaging.python.org/en/latest/glossary/#term-Wheel
# and https://realpython.com/python-wheels/ for more information.
# This script is called from our GHA build-xxx workflows.
# It assumes the semgrep-core binary has been copied under cli/src/semgrep/bin
# for pip to package semgrep correctly.

set -e
pip3 install setuptools wheel
cd cli && python3 setup.py sdist bdist_wheel "$@"

# We have to tweak our architecture-specific Python wheels in order for them to be accepted by PyPI.
# The output of setup.py bdist_wheel is a file formatted like this: (example is for amd64, but arm64 is similar)
# semgrep-1.32.0-cp37.cp38.cp39.cp310.cp311.py37.py38.py39.py310.py311-none-linux_x86_64.whl
#
# linux_x64_64 is the wheel's "platform tag." PyPI doesn't allow it because it's non-standard
#
# The solution here is to re-tag by making copies of the wheel with "proper" platform tags:
# - manylinux2014_x86_64 for glibc platforms (e.g. Debian)
# - musllinux_1_0_x86_64 for musl platforms (e.g. Alpine)
#
# Check out https://packaging.python.org/en/latest/specifications/platform-compatibility-tags/ for more info about platform tags
#

for wheel_filename in dist/*.whl; do
    if [[ ! $wheel_filename =~ ^(.*-)linux(_[a-z0-9_]+\.whl)$ ]]; then
        echo "Skipping wheel: $wheel_filename"
        continue
    fi

    manylinux_filename="${BASH_REMATCH[1]}manylinux2014${BASH_REMATCH[2]}"
    musllinux_filename="${BASH_REMATCH[1]}musllinux_1_0${BASH_REMATCH[2]}"

    cp -v "$wheel_filename" "$manylinux_filename"
    cp -v "$wheel_filename" "$musllinux_filename"
    rm "$wheel_filename"
done

# Zipping for a stable name to upload as an artifact
zip -r dist.zip dist
