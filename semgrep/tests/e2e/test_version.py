import pytest
import re
import subprocess


@pytest.mark.kinda_slow
def test_version():
    result = subprocess.check_output(
        ["semgrep", "--version", "--disable-version-check"], encoding="utf-8"
    )

    assert re.match(r"\d+\.\d+\.\d+", result)
