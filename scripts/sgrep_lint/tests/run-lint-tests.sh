#!/bin/bash

set -e
THIS_DIR="$(dirname "$(realpath "$0")")";

cd ${THIS_DIR}/../../
pytest sgrep_lint/

cd ${THIS_DIR}/../../;
SGREP="python3 -m sgrep_lint.sgrep"
$SGREP --json --strict --config sgrep_lint/tests/python/eqeq.yaml sgrep_lint/tests/lint -o tmp.out >/dev/null
diff tmp.out sgrep_lint/tests/python/eqeq.expected.json
rm -f tmp.out

# parsing bad.yaml should fail 
$SGREP --strict --config sgrep_lint/tests/python/bad.yaml sgrep_lint/tests/lint && echo "bad.yaml should have failed" && exit 1

# parsing bad2.yaml should fail 
$SGREP --strict --config sgrep_lint/tests/python/bad2.yaml sgrep_lint/tests/lint && echo "bad2.yaml should have failed" && exit 1

# parsing bad3.yaml should fail 
$SGREP --strict --config sgrep_lint/tests/python/bad3.yaml sgrep_lint/tests/lint && echo "bad3.yaml should have failed" && exit 1


echo "-----------------------"
echo "all lint tests passed"