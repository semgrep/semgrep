#!/usr/bin/env bash

# Libcurl is the library for curl. Semgrep depends on it at runtime because it
# uses an backend relying on curl to send OpenTelemetry traces.
#
# This script is necessary when building Semgrep in Alpine, since installing it
# via apk add causes problems build against some of curl's dependencies. It's
# easier to just download and build it ourselves.
# TODO: is this still true with our switch to Alpine 3.19?

set -eu

CURL_VERSION="8.5.0"

cd /tmp

curl -L "https://curl.se/download/curl-${CURL_VERSION}.tar.gz" | tar xz

cd /tmp/curl-${CURL_VERSION}

# Jan 2025: Disabling libpsl (a cookie checking library apparently) because it started
# to cause linking errors later in semgrep about a "missing -lpsl"
./configure --disable-shared --with-ssl --disable-ldap --without-brotli --without-nghttp2 --without-libidn2 --without-libpsl

make install
