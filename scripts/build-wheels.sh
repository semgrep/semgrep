#! /usr/bin/env bash
set -e
pip install setuptools wheel
cd semgrep && python setup.py sdist bdist_wheel
# Zipping for a stable name to upload as an artifact
zip -r dist.zip dist
