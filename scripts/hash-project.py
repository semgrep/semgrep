#!/usr/bin/env python3
from sys import argv

from semgrep.metric_manager import metric_manager

metric_manager.set_project_hash(argv[1])
print(metric_manager._project_hash)
