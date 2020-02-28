#!/bin/bash

set -e

echo "-----------------------"
echo "starting lint tests"

THIS_DIR="$(dirname "$(realpath "$0")")";

cd "${THIS_DIR}"
PYTHONPATH=.. pytest .

cd "${THIS_DIR}/../";
SGREP="./sgrep.py"
$SGREP --json --strict --config tests/python/eqeq.yaml tests/lint -o tmp.out >/dev/null
diff tmp.out tests/python/eqeq.expected.json
rm -f tmp.out

# test relative paths
$SGREP --json --strict --config ../sgrep_lint/tests/python/eqeq.yaml tests/lint -o tmp.out >/dev/null
diff tmp.out tests/python/eqeq.expected.relative.json
rm -f tmp.out

# test absolute directory
cp tests/python/eqeq.yaml /tmp
$SGREP --json --strict --config /tmp/eqeq.yaml tests/lint -o tmp.out >/dev/null
diff tmp.out tests/python/eqeq.expected.directory.json
rm -f tmp.out
rm -f /tmp/eqeq.yaml


# test url paths
$SGREP --json --strict --config=https://raw.githubusercontent.com/returntocorp/sgrep-rules/develop/template.yaml tests/lint -o tmp.out >/dev/null
diff tmp.out tests/python/eqeq.expected.remote.json
rm -f tmp.out

# test registry
$SGREP --json --strict --config=r2c tests/lint -o tmp.out >/dev/null
diff tmp.out tests/python/eqeq.expected.registry.json
rm -f tmp.out

# test .sgrep.yml
rm -rf .sgrep.yml
$SGREP --generate-config
$SGREP --json --strict tests/lint -o tmp.out >/dev/null
diff tmp.out tests/python/eqeq.expected.template.json
rm -f tmp.out
rm -rf .sgrep.yml

# test .sgrep/ directory
rm -rf .sgrep/ && mkdir .sgrep/
$SGREP --generate-config
mv .sgrep.yml .sgrep/
$SGREP --json --strict tests/lint -o tmp.out >/dev/null
diff tmp.out tests/python/eqeq.expected.template.json
rm -f tmp.out
rm -rf .sgrep/

# parsing bad.yaml should fail
$SGREP --strict --config tests/python/bad.yaml tests/lint && echo "bad.yaml should have failed" && exit 1

# parsing bad2.yaml should fail
$SGREP --strict --config tests/python/bad2.yaml tests/lint && echo "bad2.yaml should have failed" && exit 1

# parsing bad3.yaml should fail
$SGREP --strict --config tests/python/bad3.yaml tests/lint && echo "bad3.yaml should have failed" && exit 1

echo "-----------------------"
echo "all lint tests passed"
