#!/bin/bash

set -e
cd "$(dirname "$(realpath "$0")")";

PYTHONPATH=.. python3 test_lint.py

cd ..
./sgrep.py testlint/python tests/lint > tmp.out
diff tmp.out ./testlint/python/eqeq.expected.json
rm -f tmp.out

echo "-----------------------"
echo "all lint tests complete"