from pathlib import Path

DASHBOARD_URL = "https://dashboard.semgrep.dev"
STATS_URL = "https://stats.semgrep.dev"
SEMGREP_URL = "https://semgrep.dev"
BPS_ENDPOINT = "semgrep.perf.bps"
LPS_ENDPOINT = "semgrep.perf.lps"

STD = "std"

SEMGREP_USER_AGENT = "Semgrep/0.0.0-benchmark"
RULE_CONFIG_CACHE_DIR = Path("rules_cache")
PREP_FILE_TEMPLATE = """#! /bin/sh
#
# Fetch rules and targets prior for the "r2c" benchmark.
#
# rule_dir: input/rules
# target_dir: input/socket.io
#
# See ../r2c-rules for centralized copy of the rule
# Uses sh because bash is not installed within the semgrep docker container.
#
set -eu

mkdir -p input
mkdir -p input/rules

cp -r {rule_cache_dir} input/rules
cd input

# Obtain a shallow clone of a git repo for a specific commit
shallow_clone() {{
  if [ -d "$name" ]; then
    echo "Reusing repo '$name'"
  else
    echo "Obtaining a shallow clone of git repo '$name', commit $commit"
    mkdir -p "$name"
    (
      cd "$name"
      git init
      git remote add origin "$url"
      git fetch --depth 1 origin "$commit"
      git checkout FETCH_HEAD -b tmp
    )
  fi
}}

# Targets using other repos we run in CI
name="{name}"
url="{url}"
commit="{commit_hash}"
shallow_clone
"""
