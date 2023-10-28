import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.osempass
@pytest.mark.kinda_slow
@pytest.mark.parametrize("max_bytes", ["1MB"])
def test_max_target_bytes(run_semgrep_in_tmp: RunSemgrep, snapshot, max_bytes):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic",
        options=["--max-target-bytes", max_bytes],
        assert_exit_code=None,
    )
    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.kinda_slow
@pytest.mark.parametrize("max_bytes", ["1B", "1.3R"])
@pytest.mark.osemfail
def test_max_target_bytes_osemfail(run_semgrep_in_tmp: RunSemgrep, snapshot, max_bytes):
    test_max_target_bytes(run_semgrep_in_tmp, snapshot, max_bytes)
