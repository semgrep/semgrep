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

test_semgrep_local () {
    cd "${THIS_DIR}/../";
    $SGREP --json --strict --config tests/python/eqeq.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.json
    rm -f tmp.out
}

test_semgrep_relative() {
    # test relative paths
    cd "${THIS_DIR}/../";
    $SGREP --json --strict --config ../semgrep/tests/python/eqeq.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.relative.json
    rm -f tmp.out
}

test_semgrep_absolute() {
    cd "${THIS_DIR}/../";
    cp tests/python/eqeq.yaml /tmp
    $SGREP --json --strict --config /tmp/eqeq.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.directory.json
    rm -f tmp.out
    rm -f /tmp/eqeq.yaml
}

test_semgrep_url_config() {
    cd "${THIS_DIR}/../";
    # test url paths
    $SGREP --json --strict --config=https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.remote.json
    rm -f tmp.out
}

test_registry() {
    $SGREP --json --strict --config=r2c tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.registry.json
    rm -f tmp.out
}

test_semgrep_default_file() {
    cd "${THIS_DIR}/../";
    # test .semgrep.yml
    rm -rf .semgrep.yml
    $SGREP --generate-config
    $SGREP --json --strict tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.template.json
    rm -f tmp.out
    rm -rf .semgrep.yml
}

test_semgrep_default_folder() {
    cd "${THIS_DIR}/../";
    # test .semgrep/ directory
    rm -rf .semgrep/ && mkdir .semgrep/
    $SGREP --generate-config
    mv .semgrep.yml .semgrep/
    $SGREP --json --strict tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.template.json
    rm -f tmp.out
    rm -rf .semgrep/
}


echo "-----------------------"
echo "starting lint tests"

THIS_DIR="$(dirname "$(realpath "$0")")";

cd "${THIS_DIR}"
PYTHONPATH=.. pytest .

local_tests() {
    SGREP="python3 -m semgrep"
    test_semgrep_local
    test_semgrep_relative
    test_semgrep_absolute
    test_semgrep_url_config
    test_registry
    test_semgrep_default_file
    test_semgrep_default_folder
}

docker_tests() {
    SGREP="docker run --rm -v \"\${PWD}:/home/repo\" returntocorp/semgrep:develop"
    test_semgrep_local
    #test_semgrep_relative
    #test_semgrep_absolute
    test_semgrep_url_config
    test_registry
    test_semgrep_default_file
    test_semgrep_default_folder
}

local_tests
#echo "semgrep docker develop image"
#docker_tests

# parsing bad.yaml should fail
$SGREP --strict --config tests/python/bad.yaml tests/lint && echo "bad.yaml should have failed" && exit 1

# parsing badpattern.yaml should fail
$SGREP --strict --config tests/python/badpattern.yaml tests/lint && echo "badpattern.yaml should have failed" && exit 1

# parsing bad2.yaml should fail
$SGREP --strict --config tests/python/bad2.yaml tests/lint && echo "bad2.yaml should have failed" && exit 1

# parsing bad3.yaml should fail
$SGREP --strict --config tests/python/bad3.yaml tests/lint && echo "bad3.yaml should have failed" && exit 1

# parsing bad4.yaml should fail
$SGREP --strict --config tests/python/bad4.yaml tests/lint && echo "bad4.yaml should have failed" && exit 1

# parsing good.yaml should succeed
$SGREP --strict --config=tests/python/good.yaml tests/lint

# parsing good_info_severity.yaml should succeed
$SGREP --strict --config=tests/python/good_info_severity.yaml tests/lint

#echo TODO: disabled semgrep-rules regression testing for now
rm -rf /tmp/semgrep-rules && git clone https://github.com/returntocorp/semgrep-rules /tmp/semgrep-rules
$SGREP --dangerously-allow-arbitrary-code-execution-from-rules --strict --test --test-ignore-todo /tmp/semgrep-rules
$SGREP --validate --config=/tmp/semgrep-rules

echo "-----------------------"
echo "all lint tests passed"
