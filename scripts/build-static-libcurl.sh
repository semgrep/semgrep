#!/usr/bin/env bash

# Libcurl is the library for curl. Semgrep depends on it at runtime because it
# uses an backend relying on curl to send OpenTelemetry traces.
#
# This script is necessary when building Semgrep in Alpine, since installing it
# via apk add causes problems build against some of curl's dependencies. It's
# easier to just download and build it ourselves.

set -eu

CURL_VERSION="8.5.0"

cd /tmp

curl -L "https://curl.se/download/curl-${CURL_VERSION}.tar.gz" | tar xz

cd /tmp/curl-${CURL_VERSION}

./configure --disable-shared --with-ssl --disable-ldap --without-brotli --without-nghttp2 --without-libidn2

make install
