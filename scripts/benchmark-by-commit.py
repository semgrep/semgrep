#!/usr/bin/env python
from __future__ import annotations

import subprocess
import sys
import time

subprocess.call(["git", "checkout", sys.argv[1]])
for _ in range(int(sys.argv[2])):
    print(
        subprocess.check_output(
            ["git", "show", "--summary", "--oneline"],
            stderr=subprocess.DEVNULL,
        )
        .decode()
        .strip(),
        end=", ",
    )
    s = time.time()
    subprocess.check_call(
        ["semgrep", "--config=p/default", "--metrics=off"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    print(time.time() - s)
    subprocess.check_call(
        ["git", "checkout", "HEAD^"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
