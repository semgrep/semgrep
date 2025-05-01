import re
import subprocess

import pytest
from tests.semgrep_runner import mk_semgrep_base_command


@pytest.mark.kinda_slow
def test_version():
    cmd = mk_semgrep_base_command("--version", ["--disable-version-check"])
    result = subprocess.check_output(
        cmd,
        encoding="utf-8",
    )

    assert re.match(r"\d+\.\d+\.\d+", result)
