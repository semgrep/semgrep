#! /usr/bin/env bash

set e

baseline_version=1.14.0
# TODO: to make local dev smoother, check if `which semgrep-core-proprietary` exists
# and use that if present
semgrep_pro_path=$(pwd)"/semgrep-core-proprietary"
semgrep_pro_path="/Users/emma/.opam/4.14.0/bin/semgrep-core-proprietary"
echo $semgrep_pro_path

cp tests/perf/deepsemgrep-sqli-rules.yaml semgrep/perf/rules

cd semgrep || return
cd cli || return

config_path=../perf/configs/ci_interfile_small_repos.yaml

# Run timing benchmark
pipenv install semgrep==$baseline_version
pipenv run python -m semgrep --version
pipenv run python -m semgrep install-semgrep-pro
export PATH=/github/home/.local/bin:$PATH

pipenv run python3 ../perf/run-benchmarks --config $config_path --std-only --save-to baseline_timing1.json --no-time
jq . baseline_timing1.json
pipenv run python3 ../perf/run-benchmarks --config $config_path --std-only --save-to baseline_timing2.json --no-time
jq . baseline_timing2.json
pipenv uninstall -y semgrep

# Install latest
SEMGREP_CORE_BIN=$semgrep_pro_path pipenv install -e .
engine_path=$(pipenv run semgrep --dump-engine-path --pro)
cp $semgrep_pro_path $engine_path

# Run latest timing benchmark
pipenv run python -m semgrep --version
pipenv run python3 ../perf/run-benchmarks --config $config_path --std-only --save-to timing1.json
jq . timing1.json
pipenv run python3 ../perf/run-benchmarks --config $config_path --std-only --save-to timing2.json --save-findings-to ci_interfile_small_repos_findings.json
jq . timing2.json
jq . ci_interfile_small_repos_findings.json

# Compare timing infos
# Note: if the snapshot files doesn't exist, you'll need to manually create it
../perf/compare-perf baseline_timing1.json baseline_timing2.json timing1.json timing2.json "$1" "$2"
../perf/compare-bench-findings ci_interfile_small_repos_findings.json
