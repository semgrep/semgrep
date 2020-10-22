import tempfile
from pathlib import Path

import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/spacegrep/terraform.yaml", "spacegrep/terraform.tf"),
        ("rules/spacegrep/html.yaml", "spacegrep/html.mustache"),
        ("rules/spacegrep/markdown.yaml", "spacegrep/markdown.md"),
        ("rules/spacegrep/httpresponse.yaml", "spacegrep/httpresponse.txt"),
    ],
)
def test_spacegrep(run_semgrep_in_tmp, snapshot, rule, target):
    # Yes, this is fugly. I apologize. T_T
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target), "results.json",
    )
