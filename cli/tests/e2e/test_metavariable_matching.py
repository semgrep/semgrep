import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_equivalence(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/inside.yaml", target_name="basic").stdout,
        "results.json",
    )
