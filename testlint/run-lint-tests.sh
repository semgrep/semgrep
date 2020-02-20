#!/bin/bash

set -e
cd "$(dirname "$(realpath "$0")")";

PYTHONPATH=.. python3 test_lint.py

cd ..
./sgrep.py --json --strict --config testlint/python/eqeq.yaml tests/lint -o tmp.out
diff tmp.out ./testlint/python/eqeq.expected.json
rm -f tmp.out

# parsing bad.yaml should fail 
./sgrep.py --strict --config testlint/python/bad.yaml tests/lint && exit 1


echo "-----------------------"
echo "all lint tests complete"