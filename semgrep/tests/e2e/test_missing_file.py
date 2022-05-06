from subprocess import CalledProcessError

import pytest

from tests.conftest import _clean_stdout


@pytest.mark.kinda_slow
def test_missing_file(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/nosem.yaml", target_name="stupid-does-not-exist.p")
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(excinfo.value.stdout), "error.json")
