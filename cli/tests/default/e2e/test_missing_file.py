import pytest
from tests.conftest import _clean_stdout
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep


# Check that a missing explicit target results in the following:
# - an error message explaining that the file is missing;
# - an error (in JSON) explaining that the file is missing;
# - a nonzero exit code.
#
@pytest.mark.kinda_slow
@pytest.mark.osemfail
@skip_on_windows  # path is substring of JSON value
def test_missing_file(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/nosem.yaml", target_name="stupid-does-not-exist.p", assert_exit_code=2
    )
    snapshot.assert_match(stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(stdout), "error.json")
