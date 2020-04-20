#!/bin/bash
set -e
mkdir -p artifacts
echo "semgrep" > artifacts/semgrep
zip -r artifacts.zip artifacts
