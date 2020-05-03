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
    $SEMGREP --json --strict --config tests/python/eqeq.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.json
    rm -f tmp.out
}

test_semgrep_relative() {
    # test relative paths
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config ../semgrep/tests/python/eqeq.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.relative.json
    rm -f tmp.out
}

test_semgrep_absolute() {
    cd "${THIS_DIR}/../";
    cp tests/python/eqeq.yaml /tmp
    $SEMGREP --json --strict --config /tmp/eqeq.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.directory.json
    rm -f tmp.out
    rm -f /tmp/eqeq.yaml
}

# Explicitly included hidden configs should be included
test_semgrep_explicit_hidden() {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config tests/python/hidden/.hidden tests/lint -o tmp.out >/dev/null
    if [ -z "$OVERRIDE_EXPECTED" ]; then
        diff tmp.out tests/python/eqeq.expected.explicit-hidden.json
    else
        cat tmp.out > tests/python/eqeq.expected.explicit-hidden.json
    fi
    rm -f tmp.out
}

# Implicitly included hidden configs should be excluded
test_semgrep_implicit_hidden() {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config tests/python/hidden tests/lint 2>tmp.out |:
    if [ -z "$OVERRIDE_EXPECTED" ]; then
        diff tmp.out tests/python/eqeq.expected.implicit-hidden.out
    else
        cat tmp.out > tests/python/eqeq.expected.implicit-hidden.out
    fi
    rm -f tmp.out
}

test_semgrep_url_config() {
    cd "${THIS_DIR}/../";
    # test url paths
    $SEMGREP --json --strict --config=https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.remote.json
    rm -f tmp.out
}

test_registry() {
    $SEMGREP --json --strict --config=r2c tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.registry.json
    rm -f tmp.out
}

test_semgrep_default_file() {
    cd "${THIS_DIR}/../";
    # test .semgrep.yml
    rm -rf .semgrep.yml
    $SEMGREP --generate-config
    $SEMGREP --json --strict tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.template.json
    rm -f tmp.out
    rm -rf .semgrep.yml
}

test_semgrep_default_folder() {
    cd "${THIS_DIR}/../";
    # test .semgrep/ directory
    rm -rf .semgrep/ && mkdir .semgrep/
    $SEMGREP --generate-config
    mv .semgrep.yml .semgrep/
    $SEMGREP --json --strict tests/lint -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/eqeq.expected.template.json
    rm -f tmp.out
    rm -rf .semgrep/
}

test_semgrep_equivalence() {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config=tests/equivalence-tests/open-redirect.equiv.yml tests/equivalence-tests -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/equivalence-tests/open-redirect.equiv.expected.json
    rm -f tmp.out
}

test_semgrep_autofix() {
    cd "${THIS_DIR}/../";
    $SEMGREP --json --strict --config=tests/python/autofix.yml tests/lint/autofix.py -o tmp.out >/dev/null
    assert_output_equal tmp.out tests/python/autofix.expected.json
    rm -f tmp.out
}

echo "-----------------------"
echo "starting lint tests"

THIS_DIR="$(dirname "$(realpath "$0")")";

cd "${THIS_DIR}"
PYTHONPATH=.. pytest .

local_tests() {
    SEMGREP="python3 -m semgrep"
    test_semgrep_local
    test_semgrep_relative
    test_semgrep_absolute
    test_semgrep_explicit_hidden
    test_semgrep_implicit_hidden
    test_semgrep_url_config
    test_registry
    test_semgrep_default_file
    test_semgrep_default_folder
    test_semgrep_equivalence
    test_semgrep_autofix
}

docker_tests() {
    SEMGREP="docker run --rm -v \"\${PWD}:/home/repo\" returntocorp/semgrep:develop"
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
