#! /usr/bin/env bash
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

chmod +x semgrep-files/semgrep-core
tar -cvzf artifacts.tar.gz semgrep-files/
