#!/bin/bash

set -e



echo "-----------------------"
echo "starting lint tests"

THIS_DIR="$(dirname "$(realpath "$0")")";

cd "${THIS_DIR}"
PYTHONPATH=.. pytest .

SEMGREP="python3 -m semgrep"

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
