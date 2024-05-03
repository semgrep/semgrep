import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "settings",
    [
        {"filename": "invalid_go.go", "rule": "eqeq-basic.yaml"},
        {"filename": "invalid_python.py", "rule": "eqeq-python.yaml"},
    ],
)
@pytest.mark.osemfail
def test_file_parser__failure__error_messages(
    run_semgrep_in_tmp: RunSemgrep, snapshot, settings
):
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


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "settings",
    [
        {"filename": "basic_java.java", "rule": "bad-java-rule.yaml"},
    ],
)
@pytest.mark.osemfail
def test_rule_parser__failure__error_messages(
    run_semgrep_in_tmp: RunSemgrep, snapshot, settings
):
    stdout, stderr = run_semgrep_in_tmp(
        config=f"rules/{settings['rule']}",
        target_name=f"bad/{settings['filename']}",
        options=["--verbose", "--no-time"],
        output_format=OutputFormat.JSON,
        force_color=True,
        assert_exit_code=2,
    )
    snapshot.assert_match(stdout, "out.json")
    snapshot.assert_match(stderr, "error.txt")


# check that we report a parse error only once per file, even if we
# run the engine with two rules
# TODO: merge with the parametrize above?
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_parse_errors(run_semgrep_in_tmp: RunSemgrep, snapshot):
    _results, errors = run_semgrep_in_tmp(
        "rules/two_rules/",
        options=["--verbose"],
        target_name="bad/invalid_javascript.js",
        output_format=OutputFormat.TEXT,
        force_color=True,
        strict=False,
    )
    snapshot.assert_match(
        errors,
        "errors.txt",
    )
