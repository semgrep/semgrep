from __future__ import annotations

import re
import subprocess

import pytest

from tests.semgrep_runner import SEMGREP_BASE_COMMAND


@pytest.mark.kinda_slow()
def test_version():
    result = subprocess.check_output(
        [*SEMGREP_BASE_COMMAND, "--version", "--disable-version-check"],
        encoding="utf-8",
    )

    assert re.match(r"\d+\.\d+\.\d+", result)
