import subprocess


def test_version(benchmark):
    benchmark(subprocess.check_output, ["python3", "-m", "semgrep", "--version"])


def test_eqeq(run_semgrep_in_tmp, benchmark):
    benchmark(run_semgrep_in_tmp, "rules/eqeq.yaml", options=["--jobs", "1"])
