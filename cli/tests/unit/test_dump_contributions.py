import json
import subprocess

import pytest

from semgrep.core_runner import get_contributions
from semgrep.engine import EngineType

COMMIT_HASH = "abcd123"
COMMIT_TIMESTAMP = "2023-09-13T00:00:00"
COMMIT_AUTHOR_NAME = "Semgrep Test"
COMMIT_AUTHOR_EMAIL = "semgrep_test@test.r2c.dev"

CONTRIBUTION = {
    "commit_hash": COMMIT_HASH,
    "commit_timestamp": COMMIT_TIMESTAMP,
    "contributor": {
        "commit_author_name": COMMIT_AUTHOR_NAME,
        "commit_author_email": COMMIT_AUTHOR_EMAIL,
    },
}


@pytest.fixture()
def mock_state(mocker):
    mocked = mocker.patch("semgrep.core_runner.get_state")
    yield mocked.return_value


@pytest.fixture()
def mock_subprocess_run(mocker):
    mocked = mocker.patch("semgrep.core_runner.subprocess.run")
    yield mocked


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_dump_contributions_nominal(mocker, mock_state, mock_subprocess_run):
    proc_result = mocker.MagicMock()
    proc_result.stdout = json.dumps([CONTRIBUTION])
    mock_subprocess_run.return_value = proc_result

    contributions = get_contributions(EngineType.OSS)
    assert contributions.to_json_string() == json.dumps([CONTRIBUTION])


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
def test_dump_contributions_failed(mock_state, mock_subprocess_run):
    mock_subprocess_run.side_effect = subprocess.CalledProcessError(1, "/bin/semgrep")

    contributions = get_contributions(EngineType.OSS)
    assert contributions.value == []
