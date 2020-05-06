#!/bin/bash
set -e
$(cd semgrep && python setup.py sdist bdist_wheel)
