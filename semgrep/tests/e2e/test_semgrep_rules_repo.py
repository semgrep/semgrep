import subprocess

import pytest


def _fail_subprocess_on_error(cmd):
    output = subprocess.run(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding="utf-8",
    )

    if output.returncode != 0:
        pytest.fail(f"Failed running cmd={cmd}" + output.stdout + output.stderr)


def test_semgrep_rules_repo(run_semgrep_in_tmp):
    subprocess.check_output(
        ["git", "clone", "--depth=1", "https://github.com/returntocorp/semgrep-rules"]
    )

    _fail_subprocess_on_error(["python3", "-m", "semgrep", "--generate-config"])

    _fail_subprocess_on_error(
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
    )

    _fail_subprocess_on_error(
        ["python3", "-m", "semgrep", "--validate", "--config", "semgrep-rules"]
    )
