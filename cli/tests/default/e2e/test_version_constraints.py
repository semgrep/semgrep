import pytest
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@skip_on_windows  # better masking
def test_version_constraints(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/version-constraints.yaml", target_name="version-constraints/x.py"
        ).stdout,
        "results.json",
    )
