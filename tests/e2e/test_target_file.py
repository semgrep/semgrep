import subprocess


def test_target_file():
    """
        Check that output of passing in file is the same
        as having that file in -target-file flag
    """
    output = subprocess.check_output(["semgrep-core", "-e", "$X==$X", "-lang", "python", "tests/e2e/targets/basic.py"], encoding="utf-8")
    output2 = subprocess.check_output(["semgrep-core", "-e", "$X==$X", "-lang", "python", "-targets", "tests/e2e/target"], encoding="utf-8")
    assert output == output2


if __name__ == "__main__":
    test_target_file()
