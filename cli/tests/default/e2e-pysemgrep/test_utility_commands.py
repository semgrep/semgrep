import re
import subprocess

import pytest
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND

# used to be in e2e/test_utility_commands.py


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_dump_command_for_core():
    semgrep_core_command = subprocess.check_output(
        SEMGREP_BASE_SCAN_COMMAND
        + [
            "--config",
            "tests/default/e2e/rules/eqeq-basic.yaml",
            "tests/default/e2e/targets/basic",
            "-d",
        ],
        encoding="utf-8",
    )

    result = subprocess.run(semgrep_core_command, shell=True)

    assert result.returncode == 0


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_dump_engine():
    result = subprocess.check_output(
        SEMGREP_BASE_SCAN_COMMAND + ["--dump-engine-path"],
        encoding="utf-8",
    )

    assert re.match(r"/[\w/\_\-\.]+/semgrep-core", result)
