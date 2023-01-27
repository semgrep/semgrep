#! /usr/bin/env bash
set -e

# TODO(devops): This scripts should be consolidated with scripts/build-wheels-m1.sh other workflows are de-duped.

pip3 install setuptools wheel
cd cli && python3 setup.py sdist bdist_wheel
# Zipping for a stable name to upload as an artifact
zip -r dist.zip dist
