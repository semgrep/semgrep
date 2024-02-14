#!/usr/bin/env bash

set -euo pipefail

# This script requires to have:
#  - jsonnet (any jsonnet should work, e.g., go-jsonnet)
#  - yq (a.k.a go-yq) from https://github.com/mikefarah/yq
#    Note that many distributions come with a default yq that is not
#    as powerful as https://github.com/mikefarah/yq so follow
#    the instructions there to install yq on your machine
#
# alt: use ../../_build/install/default/bin/ojsonnet --envir --yaml $< >> $@ ...
# but ojsonnet is still buggy and some people (infra) don't have the ocaml
# toolchain installed so it's easier for them to rely on jsonnet and yq

jsonnet "$@" | yq -P
