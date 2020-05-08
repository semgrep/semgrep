#!/bin/bash
set -e
mkdir -p artifacts
echo "semgrep-core" > artifacts/semgrep-core
zip -r artifacts.zip artifacts
