from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.core_runner import CoreRunner
from semgrep.core_runner import StreamingSemgrepCore
from semgrep.engine import EngineType
from semgrep.error import SemgrepError

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
def streaming_semgrep_core():
    with patch.object(StreamingSemgrepCore, "execute") as mocked:
        yield mocked


@pytest.fixture()
def core_runner_output(streaming_semgrep_core):
    with patch.object(CoreRunner, "_extract_core_output") as mocked:
        yield mocked


@pytest.mark.quick
def test_dump_contributions_nominal(core_runner_output):
    core_runner_output.return_value = [CONTRIBUTION]

    core_runner = CoreRunner(
        jobs=1,
        engine_type=EngineType.OSS,
        timeout=1,
        max_memory=0,
        interfile_timeout=0,
        timeout_threshold=0,
        optimizations="none",
        core_opts_str=None,
    )

    contributions = core_runner.invoke_semgrep_dump_contributions()

    expected_contributor = out.Contributor(COMMIT_AUTHOR_NAME, COMMIT_AUTHOR_EMAIL)
    expected_contribution = out.Contribution(
        COMMIT_HASH, COMMIT_TIMESTAMP, expected_contributor
    )
    assert contributions.value == [expected_contribution]


@pytest.mark.quick
def test_dump_contributions_failed(core_runner_output):
    core_runner_output.side_effect = SemgrepError()

    core_runner = CoreRunner(
        jobs=1,
        engine_type=EngineType.OSS,
        timeout=1,
        max_memory=0,
        interfile_timeout=0,
        timeout_threshold=0,
        optimizations="none",
        core_opts_str=None,
    )

    contributions = core_runner.invoke_semgrep_dump_contributions()
    assert contributions.value == []
