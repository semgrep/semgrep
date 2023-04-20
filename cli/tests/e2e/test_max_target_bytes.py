import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize("max_bytes", ["1MB", "1B", "1.3R"])
def test_max_target_bytes(run_semgrep_in_tmp: RunSemgrep, snapshot, max_bytes):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic",
        options=["--max-target-bytes", max_bytes],
        assert_exit_code=None,
    )
    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")
