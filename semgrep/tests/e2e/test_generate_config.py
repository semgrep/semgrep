import subprocess
import sys


def test_generate_config(run_semgrep_in_tmp):
    subprocess.check_output([sys.executable, "-m", "semgrep", "--generate-config"])
    run_semgrep_in_tmp(".semgrep.yml")
