import pytest
import subprocess
import sys


@pytest.mark.kinda_slow
def test_generate_config(run_semgrep_in_tmp):
    subprocess.check_output(
        [
            sys.executable,
            "-m",
            "semgrep",
            "--disable-version-check",
            "--metrics",
            "off",
            "--generate-config",
        ]
    )
    run_semgrep_in_tmp(".semgrep.yml")
