import subprocess


# Running semgrep with multiple configs should fail fast if any of them have errors
def test_multi_config_fail(run_semgrep_in_tmp):
    try:
        run_semgrep_in_tmp(
            [
                "rules/multi_config_fail/error.yaml",
                "rules/multi_config_fail/no_error.yaml",
            ],
            target_name="basic/stupid.py",
        )
        raise AssertionError()
    except subprocess.CalledProcessError as e:
        assert e.returncode == 7
