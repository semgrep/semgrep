#!/bin/bash
#
# Make a semgrep release for Linux x86_64.
#
set -eu

if [[ ! -f semgrep-files/semgrep-core ]]; then
  echo "Missing semgrep-files/semgrep-core binary." 2>&1
  exit 1
fi

# We may not need all of these packages anymore since semgrep-core now
# comes pre-built.
sudo apt-get install -y --no-install-recommends \
     libcurl4-openssl-dev libexpat1-dev gettext libz-dev libssl-dev \
     build-essential autoconf musl-tools

if [[ -z "${SKIP_NUITKA+x}" ]]; then
  export PATH=/github/home/.local/bin:$PATH
  (
    cd semgrep
    sudo make all
  )
fi

mkdir -p semgrep-files
cp -r ./semgrep/build/semgrep.dist/* semgrep-files
ls semgrep-files
chmod +x semgrep-files/semgrep
chmod +x semgrep-files/semgrep-core
tar -cvzf artifacts.tar.gz semgrep-files/
