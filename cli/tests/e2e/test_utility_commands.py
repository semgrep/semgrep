import re
import subprocess

import pytest
from tests.semgrep_runner import SEMGREP_BASE_COMMAND


@pytest.mark.kinda_slow
def test_version():
    result = subprocess.check_output(
        SEMGREP_BASE_COMMAND + ["--version", "--disable-version-check"],
        encoding="utf-8",
    )

    assert re.match(r"\d+\.\d+\.\d+", result)


@pytest.mark.kinda_slow
def test_dump_command_for_core():
    semgrep_core_command = subprocess.check_output(
        SEMGREP_BASE_COMMAND
        + [
            "--config",
            "tests/e2e/rules/eqeq-basic.yaml",
            "tests/e2e/targets/basic",
            "-d",
        ],
        encoding="utf-8",
    )

    result = subprocess.run(semgrep_core_command, shell=True)

    assert result.returncode == 0


@pytest.mark.kinda_slow
def test_dump_engine():
    result = subprocess.check_output(
        SEMGREP_BASE_COMMAND + ["--dump-engine-path"],
        encoding="utf-8",
    )

    assert re.match(r"/[\w/]+/semgrep-core", result)


@pytest.mark.kinda_slow
def test_dump_engine_pro():
    result = subprocess.check_output(
        SEMGREP_BASE_COMMAND + ["--dump-engine-path", "--pro"],
        encoding="utf-8",
    )

    assert re.match(r"/[\w/]+/semgrep-core-proprietary", result)
