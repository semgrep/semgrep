#!/bin/bash
#
# Use static linking if platform allows.
#
set -eu

# Expect an opam switch name like '4.10.0+musl+static+flambda'
if [[ "$(opam switch show)" == *+static* ]]; then
  echo "(-ccopt -static)" > flags.sexp
else
  echo "( :standard )" > flags.sexp
fi
