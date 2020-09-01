#!/bin/bash
set -e

echo "Loading version from file: $(cat version)"
sed 's/^v//' version > release-version
echo "Installing via homebrew"
brew install returntocorp/semgrep/semgrep

echo "Running homebrew recipe checks"
brew test returntocorp/semgrep/semgrep


brew info semgrep --json | jq -r '.[0].installed[0].version' | tee brew-version

echo "Checking the brew package has been updated"

semgrep --version > semgrep-version
echo -n "Validating brew the version ($(cat brew-version) vs. $(cat release-version))..."
diff brew-version release-version
echo "OK!"

echo -n "Validating brew the version ($(cat semgrep-version) vs. $(cat release-version))..."
diff semgrep-version release-version
echo "OK!"
