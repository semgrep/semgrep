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

# parsing bad.yaml should fail
$SGREP --strict --config tests/python/bad.yaml tests/lint && echo "bad.yaml should have failed" && exit 1

# parsing bad2.yaml should fail
$SGREP --strict --config tests/python/bad2.yaml tests/lint && echo "bad2.yaml should have failed" && exit 1

# parsing bad3.yaml should fail
$SGREP --strict --config tests/python/bad3.yaml tests/lint && echo "bad3.yaml should have failed" && exit 1

echo "-----------------------"
echo "all lint tests passed"
