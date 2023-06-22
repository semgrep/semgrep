#!/usr/bin/env bash

# This script is used in GHA to report the number of semgrep-core tests
# we have. You can see for example the number of pattern tests we have here:
# https://dashboard.semgrep.dev/metric/semgrep.core.tests.patterns.unit-test.num
# This used to report the semgrep-core tests for each language, but now
# those tests are all agglomerated under tests/patterns/.
# This script assumes it's run from the root of the semgrep project.

set -e
HOST="https://dashboard.semgrep.dev"

cd tests
for dir in */; do
    cd "$dir"
    NUM_TESTS=$(find . -name "*.sgrep" -type f | wc -l | bc)
    TEST_CATEGORY="${dir%/}"
    curl --fail -L -X POST "$HOST/api/metric/semgrep.core.tests.$TEST_CATEGORY.unit-test.num" -d "$NUM_TESTS"
    echo
    cd ..
done
