import pytest

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "settings",
    [
        {"filename": "invalid_go.go", "rule": "eqeq-basic.yaml"},
        {"filename": "invalid_python.py", "rule": "eqeq-python.yaml"},
    ],
)
def test_rule_parser__failure__error_messages(run_semgrep_in_tmp, snapshot, settings):
    stdout, stderr = run_semgrep_in_tmp(
        config=f"rules/{settings['rule']}",
        target_name=f"bad/{settings['filename']}",
        options=["--verbose", "--no-time"],
        output_format=OutputFormat.JSON,
        force_color=True,
        assert_exit_code=3,
    )
    snapshot.assert_match(stdout, "out.json")
    snapshot.assert_match(stderr, "error.txt")
