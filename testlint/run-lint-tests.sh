#!/bin/bash

set -e
cd "$(dirname "$(realpath "$0")")";

PYTHONPATH=.. python3 test_lint.py

cd ..
./sgrep.py --json --strict --config testlint/python/eqeq.yaml tests/lint -o tmp.out
diff tmp.out ./testlint/python/eqeq.expected.json
rm -f tmp.out

# parsing bad.yaml should fail 
./sgrep.py --strict --config testlint/python/bad.yaml tests/lint && echo "bad.yaml should have failed" && exit 1

# parsing bad2.yaml should fail 
./sgrep.py --strict --config testlint/python/bad2.yaml tests/lint && echo "bad2.yaml should have failed" && exit 1


echo "-----------------------"
echo "all lint tests passed"