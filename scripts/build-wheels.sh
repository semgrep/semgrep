#!/usr/bin/env bash
# This script build "wheels", which is a format used by the Pypi package manager
# to distribute binaries (for us semgrep-core) with regular Python code.
# See https://packaging.python.org/en/latest/glossary/#term-Wheel
# and https://realpython.com/python-wheels/ for more information.
# This script is called from our GHA build-xxx workflows.
# It assumes the semgrep-core binary has been copied under cli/src/semgrep/bin
# for pip to package semgrep correctly.

set -ex
# Need latest pip otherwise twine fails to install
python3 -m pip install --upgrade pip
# Need latest versions here otherwise we end up with a malformed package where
# it marks the README as an RST file which fails to parse.
python3 -m pip install --upgrade setuptools wheel twine
cd cli && python3 setup.py sdist bdist_wheel "$@"

# Do some sanity checks on the built packages. These checks are done as part of
# uploading to pypi (in the gh-action-pypi-publish action), but we only run that
# job on actual releases. Checking here will catch malformed packages on PR
# rather than on release.
twine check dist/*.whl

# Zipping for a stable name to upload as an artifact
if [[ "$OSTYPE" == "msys" ]]; then
	# zip not available on windows CI runners
	tar czvf dist.tgz dist
else
	zip -r dist.zip dist
fi
