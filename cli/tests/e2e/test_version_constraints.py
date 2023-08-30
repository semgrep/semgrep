import pytest
from tests.fixtures import RunSemgrep

# TODO: to pass with osemgrep, we need to mask the variable bits of output
# with "<MASKED>" like it's done by SemgrepResult.as_snapshot in conftest.py.
# @pytest.mark.osempass
@pytest.mark.kinda_slow
def test_version_constraints(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/version-constraints.yaml", target_name="version-constraints/x.py"
        ).stdout,
        "results.json",
    )
