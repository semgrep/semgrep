#!/bin/bash

# This has been isolated into a separate testing script because we want to
# be able to test the output of running the src/osemgrep tests. We also want
# to run the osemgrep tests separately, because otherwise the output tends to
# get drowned in our other tests.

# fail on any failure
set -e

RED='\033[0;31m'
NC='\033[0m' # No Color

dune runtest -f --no-buffer src/tests
if ! dune runtest -f --no-buffer src/osemgrep; then
  printf "\n${RED}If expectation tests have failed, run \`dune promote\` to apply the corresponding diff.${NC}\n\n"
  exit 1
fi
