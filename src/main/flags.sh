#! /usr/bin/env bash
#
# Use static linking if platform allows.
#
set -eu

# Force the use of static linking in these scenarios:
#
# - the name of the opam switch refers to musl,
#   e.g. '4.10.0+musl+static+flambda'
# - we're on alpine, in which case the opam switch doesn't have a special
#   name. It is assumed that the reason we're on alpine is to get
#   statically-linked executables.
#
if [[ "$(opam switch show)" == *+static* || -e /etc/alpine-release ]]; then
  # The -cclib statically link in libcurl's dependencies.
  # This can be removed when we transition away from the ocurl otel collector
  echo "(-cclib -lnghttp2 -cclib -lssl -cclib -lcrypto -cclib -lz -cclib -lbrotli -cclib -lidn2 -ccopt -static -ccopt -no-pie)" > flags.sexp
else
  echo "( :standard )" > flags.sexp
fi
