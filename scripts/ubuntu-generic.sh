#!/bin/bash
set -e
version="${VERSION:?Set a version to install}"
tarball=semgrep-v$version-ubuntu-16.04.tgz

tarball_url=https://github.com/returntocorp/semgrep/releases/download/v$version/$tarball
sha_url=https://github.com/returntocorp/semgrep/releases/download/v$version/$tarball.sha256


echo "Installing tarball from $tarball_url (checksum: $sha_url)"

if ! out="$(semgrep --version 2>/dev/null)" || [[ "$out" != "$version" ]]
then
    tmpdir="$(mktemp -d)"
    trap 'rm -r "$tmpdir"' EXIT
    cd "$tmpdir"
    curl -L "$tarball_url" > "$tarball"
    sha256=$(curl -L "$sha_url" | awk '{ print $1 }')
    sha256sum -c <<< "$sha256  $tarball"
    # Be gentle on `/usr/local/lib`
    rm -rf /usr/local/lib/semgrep-files
    sudo tar --skip-old-files -xzf  "$tarball" -C /usr/local/lib/
    sudo ln -sf /usr/local/lib/semgrep-files/semgrep /usr/local/bin/semgrep
    sudo ln -sf /usr/local/lib/semgrep-files/semgrep-core /usr/local/bin/semgrep-core
fi

echo "Semgrep installed! Version: $(semgrep --version)"

echo "def silly_eq(a, b):" >> /tmp/test.py
echo " return a + b == a + b" >> /tmp/test.py

echo -n "Testing your semgrep installation..."
# shellcheck disable=SC2016
semgrep /tmp/test.py -l python -e '$X == $X' | grep 'a + b' > /dev/null || (echo "Something seems wrong with your semgrep install" && exit 1)
echo "OK! Enjoy semgrep :-)"
