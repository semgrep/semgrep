import subprocess

import pytest


def test_semgrep_rules_repo(run_semgrep_in_tmp):
    subprocess.check_output(
        ["git", "clone", "--depth=1", "https://github.com/returntocorp/semgrep-rules"]
    )
    subprocess.check_output(["python3", "-m", "semgrep", "--generate-config"])
    output = subprocess.run(
        [
            "python3",
            "-m",
            "semgrep",
            "--dangerously-allow-arbitrary-code-execution-from-rules",
            "--strict",
            "--test",
            "--test-ignore-todo",
            "semgrep-rules",
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding="utf-8",
    )

    if output.returncode != 0:
        pytest.fail("Regression in Semgrep-Rules Repo" + output.stdout + output.stderr)

    subprocess.check_output(["python3", "-m", "semgrep", "--validate", "semgrep-rules"])
