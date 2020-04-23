#!/bin/bash

set -e
# set -x

HYPERFINE="hyperfine -m 2 --warmup 0 "

A_VERSION="0.4.8"
B_VERSION="0.4.9"


test_test_suite() {
    rm -rf /tmp/semgrep-rules && git clone https://github.com/returntocorp/semgrep-rules /tmp/semgrep-rules
    ls -al /tmp/semgrep-rules
    CMDA="${SGREP_A} --dangerously-allow-arbitrary-code-execution-from-rules --strict --test --test-ignore-todo /tmp/semgrep-rules"
    CMDB="${SGREP_B} --dangerously-allow-arbitrary-code-execution-from-rules --strict --test --test-ignore-todo /tmp/semgrep-rules"
    $CMDA
    $CMDB
    $HYPERFINE --export-markdown testsuite.md "${CMDA}" "${CMDB}"
}

test_sample_repos() {
    rm -rf /tmp/sample && git clone --depth=1 https://github.com/apache/airflow /tmp/sample/
    cd /tmp/sample
    SGREP_A="docker run --rm -v ${PWD}:/home/repo returntocorp/semgrep:${A_VERSION}"
    SGREP_B="docker run --rm -v ${PWD}:/home/repo returntocorp/semgrep:${B_VERSION}"

    CMDA="${SGREP_A} --config=r2c --dangerously-allow-arbitrary-code-execution-from-rules --strict"
    CMDB="${SGREP_B} --config=r2c --dangerously-allow-arbitrary-code-execution-from-rules --strict"
    #$CMDA
    #$CMDB
    $HYPERFINE --export-markdown ${THIS_DIR}/testsuite.md "${CMDA}" "${CMDB}"
    cat ${THIS_DIR}/testsuite.md
    cd -

}

echo "-----------------------"
echo "starting perf tests"

THIS_DIR="$(dirname "$(realpath "$0")")";
cd "${THIS_DIR}"

#test_test_suite
test_sample_repos

echo "-----------------------"
echo "all per tests completed"
