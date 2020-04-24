#!/bin/bash
set -eox
curl https://api.github.com/repos/returntocorp/semgrep/releases | jq ".[] | select(.tag_name == \"$(cat version)\")" > release.json
exec ./validate-release.py
