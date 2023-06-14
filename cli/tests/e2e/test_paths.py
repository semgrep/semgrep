import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_paths(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/paths.yaml", target_name="exclude_include").stdout,
        "results.json",
    )
