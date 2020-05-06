import subprocess
from pathlib import Path

import pytest


pytestmark = pytest.mark.qa


def test_semgrep_rules_repo(run_semgrep_in_tmp):
    subprocess.check_output(
        ["git", "clone", "--depth=1", "https://github.com/returntocorp/semgrep-rules"]
    )
    subprocess.check_output(["python3", "-m", "semgrep", "--generate-config"])
    subprocess.check_output(
        [
            "python3",
            "-m",
            "semgrep",
            "--dangerously-allow-arbitrary-code-execution-from-rules",
            "--strict",
            "--test",
            "--test-ignore-todo",
            "semgrep-rules",
        ]
    )
    subprocess.check_output(["python3", "-m", "semgrep", "--validate", "semgrep-rules"])
