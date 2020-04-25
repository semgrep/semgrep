#!/bin/bash
set -eox
curl https://api.github.com/repos/rcoh/semgrep/releases | jq ".[] | select(.tag_name == \"$(cat version)\")" > release.json
exec ./release-scripts/validate-release.py
