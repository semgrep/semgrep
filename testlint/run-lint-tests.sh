#!/bin/bash

set -e
cd "$(dirname "$(realpath "$0")")";

PYTHONPATH=.. python3 test_lint.py