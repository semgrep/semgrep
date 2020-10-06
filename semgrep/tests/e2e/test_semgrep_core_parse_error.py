from subprocess import CalledProcessError

import pytest


@pytest.mark.parametrize(
    "filename", ["invalid_python.py",],
)
def test_rule_parser__failure__error_messages(run_semgrep_in_tmp, snapshot, filename):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            config="rules/eqeq-python.yaml",
            target_name=f"bad/{filename}",
            options=["--verbose"],
            output_format="text",
            stderr=True,
        )
    snapshot.assert_match(excinfo.value.output, "error.txt")
