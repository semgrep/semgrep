import pytest

# Interesting values to test:
# - low value, excludes many files
# - high value, excludes fewer files
# - invalid value
# - 0, excludes no files
#
@pytest.mark.parametrize("max_bytes", ["1MB", "1B", "1.3R", "0"])
def test_max_target_bytes(run_semgrep_in_tmp, snapshot, max_bytes):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=["--max-target-bytes", max_bytes],
        fail_on_nonzero=False,
    )
    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")
