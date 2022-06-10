import json

import pytest

from ..conftest import TESTS_PATH
from semgrep.constants import OutputFormat

syntax_dir = TESTS_PATH / "e2e" / "rules" / "syntax"
syntax_passes = [f.with_suffix("").name for f in syntax_dir.glob("good*.yaml")]
syntax_fails = [
    f.with_suffix("").name
    for f in syntax_dir.glob("*.yaml")
    if "good" not in f.name and "empty" not in f.name
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filename", syntax_passes)
def test_rule_parser__success(run_semgrep_in_tmp, snapshot, filename):
    run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filename", syntax_fails)
def test_rule_parser__failure(run_semgrep_in_tmp, snapshot, filename):
    run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml", assert_exit_code={2, 7, 8})


@pytest.mark.kinda_slow
def test_regex_with_bad_language(run_semgrep_in_tmp, snapshot):
    run_semgrep_in_tmp("rules/badlanguage.yaml", assert_exit_code=7)


@pytest.mark.kinda_slow
def test_rule_parser__empty(run_semgrep_in_tmp, snapshot):
    run_semgrep_in_tmp(f"rules/syntax/empty.yaml", assert_exit_code=7)


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filename", syntax_fails)
def test_rule_parser__failure__error_messages(run_semgrep_in_tmp, snapshot, filename):
    stdout, _ = run_semgrep_in_tmp(
        f"rules/syntax/{filename}.yaml", assert_exit_code={2, 7, 8}
    )

    json_output = json.loads(stdout)
    # Something went wrong, so there had better be an error, and we asked for JSON so there had better be some output...
    assert json_output["errors"] != []

    snapshot.assert_match(stdout, "error.json")
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/{filename}.yaml",
        options=["--force-color"],
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code={2, 7, 8},
    )

    snapshot.assert_match(stderr, "error-in-color.txt")


# https://github.com/returntocorp/semgrep/issues/1095
@pytest.mark.kinda_slow
def test_rule_parser_cli_pattern(run_semgrep_in_tmp, snapshot):
    # Check json output
    stdout, _ = run_semgrep_in_tmp(
        options=["-e", "#include<asdf><<>>><$X>", "-l", "c"], assert_exit_code=2
    )
    snapshot.assert_match(stdout, "error.json")

    # Check pretty print output
    _, stderr = run_semgrep_in_tmp(
        options=["-e", "#include<asdf><<>>><$X>", "-l", "c"],
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=2,
    )
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.kinda_slow
def test_rule_parser_error_key_name_text(run_semgrep_in_tmp, snapshot):
    # Check pretty print output
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/invalid-key-name.yml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=7,
    )
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.kinda_slow
def test_rule_parser_error_metavariable_text(run_semgrep_in_tmp, snapshot):
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/invalid-metavariable-regex.yml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=7,
    )
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.kinda_slow
def test_rule_parser_error_invalid_key_name_text(run_semgrep_in_tmp, snapshot):
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/invalid-patterns-key.yml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=7,
    )
    snapshot.assert_match(stderr, "error.txt")
