import json
import shutil
from pathlib import Path

import pytest

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
def test_duplicate_matches_indexing(run_semgrep_in_tmp, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/match_based_id/duplicates.yaml",
        target_name="match_based_id/duplicates",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(results, "results.json")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target,expect_change",
    [
        # ("rules/match_based_id/","",True)
        ("rules/match_based_id/formatting.yaml", "formatting.c", False),
        ("rules/match_based_id/formatting.yaml", "ellipse.c", False),
        ("rules/taint.yaml", "taint.py", False),
        ("rules/match_based_id/operator.yaml", "operator.c", True),
        ("rules/match_based_id/formatting.yaml", "meta-change.c", True),
        ("rules/match_based_id/join.yaml", "join.py", True),
    ],
)
def test_id_change(run_semgrep_on_copied_files, tmp_path, rule, target, expect_change):

    suffix = Path(target).suffix

    # Target file prior to edit
    before_target = tmp_path / "targets" / "match_based_id" / "before" / target
    # Target file after edit
    after_target = tmp_path / "targets" / "match_based_id" / "after" / target
    # Static file path (necessary to obtain static id)
    static_target = tmp_path / "targets" / ("_match_based_id" + suffix)

    def run_on_target(target):
        shutil.copy(target, static_target)
        before_results, _ = run_semgrep_on_copied_files(
            rule,
            target_name=static_target,
            output_format=OutputFormat.JSON,
        )
        return json.loads(before_results)["results"][0]["extra"]["fingerprint"]

    before_id = run_on_target(before_target)
    after_id = run_on_target(after_target)

    assert (after_id != before_id) == expect_change
