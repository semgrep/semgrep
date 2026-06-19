#!/usr/bin/env bash
# This script build "wheels", which is a format used by the Pypi package manager
# to distribute binaries (for us semgrep-core) with regular Python code.
# See https://packaging.python.org/en/latest/glossary/#term-Wheel
# and https://realpython.com/python-wheels/ for more information.
# This script is called from our GHA build-xxx workflows.
# It assumes the semgrep-core binary has been copied under cli/src/semgrep/bin
# for pip to package semgrep correctly. And that `uv` is installed

set -ex

# Invoke `uv` to build the wheel, which invokes the build backend (`setup.py`)
cd cli
uv sync --locked
uv build --wheel

# Do some sanity checks on the built packages. These checks are done as part of
# uploading to pypi (in the gh-action-pypi-publish action), but we only run that
# job on actual releases. Checking here will catch malformed packages on PR
# rather than on release.
uv tool run twine check dist/*.whl
