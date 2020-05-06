#!/bin/bash

set -e



echo "-----------------------"
echo "starting lint tests"

THIS_DIR="$(dirname "$(realpath "$0")")";

cd "${THIS_DIR}"
PYTHONPATH=.. pytest .

SEMGREP="python3 -m semgrep"

#echo TODO: disabled semgrep-rules regression testing for now
rm -rf /tmp/semgrep-rules && git clone https://github.com/returntocorp/semgrep-rules /tmp/semgrep-rules
$SEMGREP --dangerously-allow-arbitrary-code-execution-from-rules --strict --test --test-ignore-todo /tmp/semgrep-rules
$SEMGREP --validate --config=/tmp/semgrep-rules

echo "-----------------------"
echo "all lint tests passed"
