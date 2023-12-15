import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_confidence_high(run_semgrep_in_tmp: RunSemgrep, snapshot):
    json_str = run_semgrep_in_tmp(
        "rules/confidence-rule.yaml", options=["--confidence", "HIGH"]
    ).stdout
    snapshot.assert_match(json_str, "results.json")


@pytest.mark.kinda_slow
def test_confidence_multiple(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/confidence-rule.yaml",
            options=["--confidence", "HIGH", "--confidence", "MEDIUM"],
        ).stdout,
        "results.json",
    )
