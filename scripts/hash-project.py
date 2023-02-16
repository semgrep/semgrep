#!/usr/bin/env python3
"""
E.g.

  pipenv run ../scripts/hash-project.py $(git ls-remote --get-url)
"""
from __future__ import annotations

from sys import argv

from semgrep.metrics import Metrics

metrics = Metrics()
metrics.set_project_hash(argv[1])
print(metrics._project_hash)
