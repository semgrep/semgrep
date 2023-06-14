import pytest
from tests.conftest import _clean_stdout
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_missing_file(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/nosem.yaml", target_name="stupid-does-not-exist.p", assert_exit_code=2
    )
    snapshot.assert_match(stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(stdout), "error.json")
