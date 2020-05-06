import subprocess
from pathlib import Path


def test_generate_config(run_semgrep):
    subprocess.check_output(["python", "-m", "semgrep", "--generate-config"])
    run_semgrep(".semgrep.yml")
