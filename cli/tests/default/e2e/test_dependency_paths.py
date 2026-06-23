#
# Copyright (c) 2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
# End-to-end snapshot tests for dependency paths in CLI output.
#
# Fixture: targets/dependency_aware/dependency_path_deep contains a yarn.lock
# with a 3-level chain -- a direct dependency "top" pulls in "mid", which pulls
# in the vulnerable transitive "victim". The rule depends-on "victim". With
# --x-dependency-paths the transitive finding carries the full introduction
# path (top -> mid -> victim, direct introducer first) in both JSON and SARIF.
import json

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

RULE = "rules/dependency_aware/dependency_path.yaml"
# reachability rule (has a code pattern) over the same fixture; index.js matches
# the transitively-introduced "victim", producing a *reachable* SCA finding
REACHABLE_RULE = "rules/dependency_aware/dependency_path_reachable.yaml"
TARGET = "dependency_aware/dependency_path_deep"


def _json_dependency_paths(results: str):
    """Stable, path/version-independent view of each finding's dependency paths."""
    data = json.loads(results)
    return [
        {
            "check_id": r["check_id"],
            "package": r["extra"]["sca_info"]["dependency_match"]["found_dependency"][
                "package"
            ],
            "transitivity": r["extra"]["sca_info"]["dependency_match"][
                "found_dependency"
            ]["transitivity"],
            "dependency_paths": r["extra"]["sca_info"]["dependency_match"].get(
                "dependency_paths"
            ),
        }
        for r in data["results"]
    ]


def _sarif_dependency_paths(results: str):
    data = json.loads(results)
    out = []
    for run in data.get("runs", []):
        for r in run.get("results", []):
            out.append(
                {
                    "ruleId": r.get("ruleId"),
                    "dependencyPaths": r.get("properties", {}).get("dependencyPaths"),
                }
            )
    return out


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_dependency_paths_deep_chain_json(
    run_semgrep_on_copied_files: RunSemgrep, snapshot
):
    # 3-level chain (top -> mid -> victim) exercises the multi-hop walk up to the
    # direct introducer, not just a single transitive edge.
    results, _errors = run_semgrep_on_copied_files(
        RULE,
        target_name=TARGET,
        # dependency-path computation lives in pysemgrep; --legacy forces it
        # (the default entrypoint dispatches to osemgrep, which does not emit it)
        options=["--legacy", "--x-dependency-paths"],
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        json.dumps(_json_dependency_paths(results), indent=2, sort_keys=True),
        "dependency_paths_deep.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_dependency_paths_deep_chain_sarif(
    run_semgrep_on_copied_files: RunSemgrep, snapshot
):
    results, _errors = run_semgrep_on_copied_files(
        RULE,
        target_name=TARGET,
        options=["--legacy", "--x-dependency-paths"],
        output_format=OutputFormat.SARIF,
    )
    snapshot.assert_match(
        json.dumps(_sarif_dependency_paths(results), indent=2, sort_keys=True),
        "dependency_paths_deep.sarif.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_dependency_paths_absent_without_flag(
    run_semgrep_on_copied_files: RunSemgrep,
):
    # the gate: without --x-dependency-paths, the field must not be emitted
    results, _errors = run_semgrep_on_copied_files(
        RULE,
        target_name=TARGET,
        options=["--legacy"],
        output_format=OutputFormat.JSON,
    )
    extracted = _json_dependency_paths(results)
    assert extracted, "expected at least one SCA finding"
    assert all(e["dependency_paths"] is None for e in extracted)


def _reachable_findings(results: str):
    return [
        e for e in _json_dependency_paths(results) if e["transitivity"] == "transitive"
    ]


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_dependency_paths_reachable_with_flag(
    run_semgrep_on_copied_files: RunSemgrep,
):
    # the reachable path (code match on the transitive "victim") must also carry
    # the introduction path when the flag is set
    results, _errors = run_semgrep_on_copied_files(
        REACHABLE_RULE,
        target_name=TARGET,
        options=["--legacy", "--x-dependency-paths"],
        output_format=OutputFormat.JSON,
    )
    reachable = _reachable_findings(results)
    assert reachable, "expected a reachable SCA finding on the transitive victim"
    assert all(e["dependency_paths"] for e in reachable)


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_dependency_paths_reachable_absent_without_flag(
    run_semgrep_on_copied_files: RunSemgrep,
):
    # regression guard: the reachable path used to gate on `parent_index is not
    # None` (always true), leaking paths without the flag. It must gate on the
    # flag, like the unreachable path.
    results, _errors = run_semgrep_on_copied_files(
        REACHABLE_RULE,
        target_name=TARGET,
        options=["--legacy"],
        output_format=OutputFormat.JSON,
    )
    reachable = _reachable_findings(results)
    assert reachable, "expected a reachable SCA finding on the transitive victim"
    assert all(e["dependency_paths"] is None for e in reachable)
