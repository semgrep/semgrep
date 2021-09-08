import pytest


@pytest.mark.parametrize("max_bytes", ["1MB", "1B", "1.3R"])
def test_max_target_bytes(run_semgrep_in_tmp, snapshot, max_bytes):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--max-target-bytes", max_bytes],
        fail_on_nonzero=False,
    )
    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")
