#!/bin/bash
## For testing release jobs, make an empty ubuntu release quickly
set -e

mkdir -p semgrep-files
echo "sgrep-core" > semgrep-files/semgrep-core
echo "sgrep-lint" > semgrep-files/semgrep
chmod +x semgrep-files/semgrep-core
chmod +x semgrep-files/semgrep
tar -cvzf artifacts.tar.gz semgrep-files/
