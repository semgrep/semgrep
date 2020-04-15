#!/bin/bash
echo "def silly_eq(a, b):" >> /tmp/test.py
echo " return a + b == a + b" >> /tmp/test.py

echo -n "Testing your semgrep installation..."
# shellcheck disable=SC2016
semgrep /tmp/test.py -l python -e '$X == $X' | grep "return a + b == a + b"
echo "OK! Enjoy semgrep :-)"
