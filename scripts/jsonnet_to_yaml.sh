#!/usr/bin/env bash

set -euo pipefail

# This script requires to have jsonnet (any jsonnet should work),
# and yq from https://github.com/mikefarah/yq
#
# alt: use ../../_build/install/default/bin/ojsonnet --envir --yaml $< >> $@ ...
# but ojsonnet is still buggy and some people (infra) don't have the ocaml
# toolchain installed so it's easier for them to rely on jsonnet and yq

jsonnet "$@" | yq -P
