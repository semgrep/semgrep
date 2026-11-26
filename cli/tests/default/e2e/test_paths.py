#
# Copyright (c) 2020-2025 Semgrep Inc.
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
import json
from pathlib import Path

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# Regression test for https://github.com/semgrep/semgrep/issues/11560
# When a single file is passed as a scanning root, rule paths.include
# filtering should still work. Before the fix, the ppath was set to just
# the filename (e.g. "/included.js") instead of the full project-relative
# path, so the "included/" pattern in the directory-included rule's
# paths.include never matched.
@pytest.mark.kinda_slow
def test_paths_single_file_target(run_semgrep_in_tmp: RunSemgrep):
    result = run_semgrep_in_tmp(
        "rules/paths.yaml",
        target_name="exclude_include/included/included.js",
    )
    results = json.loads(result.stdout)["results"]
    matched_rule_ids = {r["check_id"] for r in results}
    assert any(rid.endswith("directory-included") for rid in matched_rule_ids), (
        f"paths.include filtering broken for single-file target; "
        f"matched rules: {matched_rule_ids}"
    )


# Test in-rule path filtering: paths.include, paths.exclude.
# The patterns now obey the Semgrepignore/Gitignore spec.
#
@pytest.mark.kinda_slow
def test_paths(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp("rules/paths.yaml", target_name="exclude_include").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filter_kind", ["include", "exclude"])
@pytest.mark.parametrize(
    "pattern, matching_paths",
    [
        ("/test.py", {"test.py"}),
        ("test.py", {"test.py", "a/test.py", "x/a/test.py"}),
        ("a/test.py", {"a/test.py"}),
        ("/a/test.py", {"a/test.py"}),
        ("**/a/test.py", {"a/test.py", "x/a/test.py"}),
        ("a/", {"a/test.py", "x/a/test.py"}),
    ],
)
def test_rule_path_anchoring(
    run_semgrep: RunSemgrep,
    tmp_path: Path,
    monkeypatch,
    filter_kind,
    pattern,
    matching_paths,
):
    monkeypatch.chdir(tmp_path)
    all_paths = {"test.py", "a/test.py", "x/a/test.py"}
    for path in all_paths:
        target = Path(path)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text("hello\n")
    config = Path("rule.yaml")
    config.write_text(
        "rules:\n"
        "- id: anchored-paths\n"
        "  pattern: hello\n"
        "  languages: [python]\n"
        "  message: found something\n"
        "  severity: ERROR\n"
        f"  paths:\n    {filter_kind}: [{json.dumps(pattern)}]\n"
    )
    result = run_semgrep(
        config,
        target_name=".",
        assume_targets_dir=False,
        output_format=OutputFormat.JSON,
    )
    findings = json.loads(result.stdout)["results"]
    expected_paths = (
        matching_paths if filter_kind == "include" else all_paths - matching_paths
    )
    assert {Path(finding["path"]) for finding in findings} == {
        Path(path) for path in expected_paths
    }
    assert "will soon be interpreted" not in result.stderr
