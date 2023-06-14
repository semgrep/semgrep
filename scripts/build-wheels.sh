#!/usr/bin/env bash
# This script build "wheels" which is a format used by the Pypi package manager.
# See https://realpython.com/python-wheels/

set -e
pip3 install setuptools wheel
cd cli && python3 setup.py sdist bdist_wheel
# Zipping for a stable name to upload as an artifact
zip -r dist.zip dist
