#!/usr/bin/env bash

# Libcurl is the library for curl. Semgrep depends on it at runtime because it
# uses a backend relying on curl to send OpenTelemetry traces.
#
# We build libcurl from source rather than using Alpine's curl-static package
# because the system libcurl.a is built with many optional features enabled
# (brotli, zstd, libpsl, libidn2, nghttp2, nghttp3, c-ares, etc.), each of
# which adds a transitive static link dependency. Our static link flags in
# flags.sh only list -lssl -lcrypto -lz, so the extra deps cause unresolved
# symbol errors at link time. Building from source with those features disabled
# produces a minimal libcurl.a that only needs OpenSSL and zlib.
#
# Last verified: Alpine 3.23 (Apr 2026) still has this problem.
# `pkg-config --static --libs libcurl` on Alpine 3.23 outputs:
#   -lcurl -lssl -lcrypto -lz -lbrotlidec -lbrotlicommon -lzstd -pthread
#   -lssl -lcrypto -ldl -pthread -lpsl -lunistring -lidn2 -lunistring
#   -lnghttp2 -lnghttp3 -lcares

set -eu

if [[ "${FORCE_DYNLINK-}" == "true" ]]; then
    echo "FORCE_DYNLINK is set, skipping static libcurl build"
elif [[ -e /etc/alpine-release ]]; then
    CURL_VERSION="8.5.0"
    # SHA-256 of the official release tarball from https://curl.se/download/
    # To update: download the new tarball and run `shasum -a 256 curl-<ver>.tar.gz`
    CURL_SHA256="05fc17ff25b793a437a0906e0484b82172a9f4de02be5ed447e0cab8c3475add"

    ALPINE_APK_DEPS=(pkgconf openssl-dev openssl-libs-static zlib-static)
    apk add "${ALPINE_APK_DEPS[@]}"
    cd /tmp

    curl -L -o "curl-${CURL_VERSION}.tar.gz" "https://curl.se/download/curl-${CURL_VERSION}.tar.gz"

    echo "${CURL_SHA256}  curl-${CURL_VERSION}.tar.gz" | sha256sum -c - \
        || { echo "ERROR: Checksum mismatch for curl-${CURL_VERSION}.tar.gz! Aborting."; exit 1; }

    tar xz < "curl-${CURL_VERSION}.tar.gz"

    cd /tmp/curl-${CURL_VERSION}

    # Jan 2025: Disabling libpsl (a cookie checking library apparently) because it started
    # to cause linking errors later in semgrep about a "missing -lpsl"
    #
    # disable zstd since it does not play with linking, we haven't built curl
    # with it before, but it's used by default if another dependency installs
    # zstd (such an ocaml or python library)
    ./configure --disable-shared --with-ssl --disable-ldap --without-zstd --without-brotli --without-nghttp2 --without-libidn2 --without-libpsl

    make install
else
    echo "Not an Alpine system, skipping libcurl build"
fi
