import pytest

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "filename",
    [
        "invalid_python.py",
    ],
)
def test_rule_parser__failure__error_messages(run_semgrep_in_tmp, snapshot, filename):
    _, stderr = run_semgrep_in_tmp(
        config="rules/eqeq-python.yaml",
        target_name=f"bad/{filename}",
        options=["--verbose"],
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=3,
    )
    snapshot.assert_match(stderr, "error.txt")
