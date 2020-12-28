#! /usr/bin/env bash
set -e
version="${VERSION:?Set a version to install}"
tarball=semgrep-v$version-ubuntu-16.04.tgz

tarball_url=https://github.com/returntocorp/semgrep/releases/download/v$version/$tarball
sha_url=https://github.com/returntocorp/semgrep/releases/download/v$version/$tarball.sha256


echo "Installing tarball from $tarball_url (checksum: $sha_url)"

tmpdir="$(mktemp -d)"
trap 'rm -r "$tmpdir"' EXIT
cd "$tmpdir"
curl -L "$tarball_url" > "$tarball"
sha256=$(curl -L "$sha_url" | awk '{ print $1 }')
sha256sum -c <<< "$sha256  $tarball"
