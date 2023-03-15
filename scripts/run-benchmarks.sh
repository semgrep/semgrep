#! /usr/bin/env bash

set e

cd cli || return

# Run timing benchmark
pipenv install semgrep==1.14.0
pipenv run python -m semgrep --version
export PATH=/github/home/.local/bin:$PATH

pipenv run python3 ../perf/run-benchmarks --small-only --std-only --save-to baseline_timing1.json --no-time
jq . baseline_timing1.json
pipenv run python3 ../perf/run-benchmarks --small-only --std-only --save-to baseline_timing2.json --no-time
jq . baseline_timing2.json
pipenv uninstall -y semgrep

# Install latest
pipenv install -e .

# Run latest timing benchmark
pipenv run python -m semgrep --version
pipenv run semgrep-core -version
pipenv run python3 ../perf/run-benchmarks --small-only --std-only --save-to timing1.json
jq . timing1.json
pipenv run python3 ../perf/run-benchmarks --small-only --std-only --save-to timing2.json --save-findings-to findings.json
jq . timing2.json
jq . findings.json

# Compare timing infos
../perf/compare-perf baseline_timing1.json baseline_timing2.json timing1.json timing2.json "$1" "$2"
../perf/compare-bench-findings findings.json
