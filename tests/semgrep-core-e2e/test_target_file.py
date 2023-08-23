import subprocess

# This test assume it's run from the root.
# See 'make core-test-e2e' in the toplevel Makefile.
def test_target_file():
    """
        Check that output of passing in file is the same
        as having that file in -target-file flag
    """
    output = subprocess.check_output(["bin/semgrep-core", "-e", "$X==$X", "-lang", "python", "tests/semgrep-core-e2e/targets/basic.py"], encoding="utf-8")
    output2 = subprocess.check_output(["bin/semgrep-core", "-e", "$X==$X", "-lang", "python", "-targets", "tests/semgrep-core-e2e/target"], encoding="utf-8")
    assert output == output2


if __name__ == "__main__":
    test_target_file()
