#!/bin/bash
set -eox
curl https://api.github.com/repos/returntocorp/sgrep/releases/latest > release.json

# Look for release notes
cat release.json | jq '.body' | grep -o "Changed"
cat release.json | jq '.body' | grep -o "Added"

cat release.json | jq -r '.tag_name' | sed 's/^v//' > version
cat release.json | jq -r '.assets[].name'
echo "Looking for version: $(cat version)"

echo "Looking for ubuntu binary"
cat release.json | jq '.assets[].name' | grep "sgrep-$(cat version)-ubuntu-16.04.tgz"

echo "Looking for ubuntu checksum"
cat release.json | jq '.assets[].name' | grep "sgrep-$(cat version)-ubuntu-16.04.tgz.sha256"

echo "Looking for OSX binary"
cat release.json | jq '.assets[].name' | grep "sgrep-$(cat version)-osx.zip"

echo "Validating Ubuntu checksum"
SHA_URL="$(cat release.json | jq -r '.assets[].browser_download_url' | grep "sha256" | grep "16.04")"
RELEASE_URL="$(cat release.json | jq -r '.assets[].browser_download_url' | grep -v "sha256" | grep "16.04")"

curl -L "$SHA_URL" | awk '{ print $1 }' > expected_sha

curl -L "$RELEASE_URL" | sha256sum | awk '{ print $1 }' > actual_sha

diff -w expected_sha actual_sha
