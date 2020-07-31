import subprocess


def test_generate_config(run_semgrep_in_tmp):
    subprocess.check_output(["python3", "-m", "semgrep", "--generate-config"])
    run_semgrep_in_tmp(".semgrep.yml")
