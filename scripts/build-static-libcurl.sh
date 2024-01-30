#!/usr/bin/env bash

set -eu

CURL_VERSION="8.5.0"

cd /tmp

curl -L "https://curl.se/download/curl-${CURL_VERSION}.tar.gz" | tar xz

cd /tmp/curl-${CURL_VERSION}

./configure --disable-shared --with-ssl --disable-ldap --without-nghttp2 --without-libidn2

make install
