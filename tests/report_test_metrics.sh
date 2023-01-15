#! /usr/bin/env bash
set -e
HOST="https://dashboard.semgrep.dev"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

pushd $SCRIPT_DIR
for dir in */; do
    cd $dir
    NUM_TESTS=$(find . -name "*.sgrep" -type f | wc -l | bc)
    TEST_CATEGORY="${dir%/}"
    curl --fail -L -X POST "$HOST/api/metric/semgrep.core.tests.$TEST_CATEGORY.unit-test.num" -d $NUM_TESTS
    echo
    cd ..
done
popd