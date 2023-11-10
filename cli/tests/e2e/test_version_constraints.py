import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_version_constraints(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/version-constraints.yaml", target_name="version-constraints/x.py"
        ).stdout,
        "results.json",
    )
