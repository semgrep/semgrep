#!/usr/bin/env python3
"""
E.g.

  pipenv run scripts/hash-project.py $(git ls-remote --get-url)
"""
from sys import argv

from semgrep.app.metrics import metric_manager

metric_manager.set_project_hash(argv[1])
print(metric_manager._project_hash)
