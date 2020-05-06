#!/bin/bash

set -e

assert_output_equal () {
    actual_path=$1
    expected_path=$2
    if [ -z "$OVERRIDE_EXPECTED" ]; then
        echo "checking $expected_path"
        diff --side-by-side <(echo EXPECTED) <(echo ACTUAL) || true
        diff --side-by-side <(python -m json.tool $expected_path) <(python -m json.tool $actual_path)
    else
        echo "regenerating $expected_path"
        diff --side-by-side <(echo EXPECTED) <(echo ACTUAL) || true
        diff --side-by-side <(python -m json.tool $expected_path) <(python -m json.tool $actual_path) || true
        cat $actual_path > $expected_path
    fi
}


test_semgrep_exclude () {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config tests/python/eqeq.yaml --include '*.py' tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.include.json
    rm -f tmp.out
}

test_semgrep_include () {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config tests/python/eqeq.yaml --exclude '*.py' tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.exclude.json
    rm -f tmp.out
}

test_semgrep_exclude_dir () {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config tests/python/eqeq.yaml --exclude-dir 'excluded_dir' tests/lint tests/excluded_dir -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.exclude_dir.json
    rm -f tmp.out
}

test_semgrep_include_dir () {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config tests/python/eqeq.yaml --include-dir 'lint' tests/lint tests/excluded_dir -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.include_dir.json
    rm -f tmp.out
}



echo "-----------------------"
echo "starting lint tests"

THIS_DIR="$(dirname "$(realpath "$0")")";

cd "${THIS_DIR}"
PYTHONPATH=.. pytest .

local_tests() {
    SEMGREP="python3 -m semgrep"
    test_semgrep_exclude
    test_semgrep_include
    test_semgrep_exclude_dir
    test_semgrep_include_dir
}

local_tests

# parsing bad.yaml should fail
$SEMGREP --strict --config tests/python/bad.yaml tests/lint && echo "bad.yaml should have failed" && exit 1

# parsing badpattern.yaml should fail
$SEMGREP --strict --config tests/python/badpattern.yaml tests/lint && echo "badpattern.yaml should have failed" && exit 1

# parsing bad2.yaml should fail
$SEMGREP --strict --config tests/python/bad2.yaml tests/lint && echo "bad2.yaml should have failed" && exit 1

# parsing bad3.yaml should fail
$SEMGREP --strict --config tests/python/bad3.yaml tests/lint && echo "bad3.yaml should have failed" && exit 1

# parsing bad4.yaml should fail
$SEMGREP --strict --config tests/python/bad4.yaml tests/lint && echo "bad4.yaml should have failed" && exit 1

# parsing good.yaml should succeed
$SEMGREP --strict --config=tests/python/good.yaml tests/lint

# parsing good_info_severity.yaml should succeed
$SEMGREP --strict --config=tests/python/good_info_severity.yaml tests/lint

# parsing good_metadata.yaml should succeed
$SEMGREP --strict --config=tests/python/good_metadata.yaml tests/lint

#echo TODO: disabled semgrep-rules regression testing for now
rm -rf /tmp/semgrep-rules && git clone https://github.com/returntocorp/semgrep-rules /tmp/semgrep-rules
$SEMGREP --dangerously-allow-arbitrary-code-execution-from-rules --strict --test --test-ignore-todo /tmp/semgrep-rules
$SEMGREP --validate --config=/tmp/semgrep-rules

echo "-----------------------"
echo "all lint tests passed"
