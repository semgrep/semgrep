import json

import pytest
from tests.conftest import RULES_PATH
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

syntax_dir = RULES_PATH / "syntax"
syntax_passes = [f.with_suffix("").name for f in syntax_dir.glob("good*.yaml")]

# NOTE: We need to quarantine at least one test as it is failing with the osemgrep
# Parse_rule.ml implementation.
quarantined = {"bad12"}  # See SAF-1556

syntax_fails = [
    f.with_suffix("").name
    for f in syntax_dir.glob("*.yaml")
    if "good" not in f.name
    and "empty" not in f.name
    and f.with_suffix("").name not in quarantined
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filename", syntax_passes)
def test_rule_parser__success(run_semgrep_in_tmp: RunSemgrep, snapshot, filename):
    run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filename", syntax_fails)
def test_rule_parser__failure(run_semgrep_in_tmp: RunSemgrep, snapshot, filename):
    run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml", assert_exit_code={2, 7, 8})


# TODO: osemgrep does not return exit code 8 yet, since all errors are handled
# by Core_runner, which returns invalid_code (3) or fatal (2), using
# Cli_json_output.exit_code_of_error_type
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_regex_with_bad_language(run_semgrep_in_tmp: RunSemgrep, snapshot):
    run_semgrep_in_tmp("rules/syntax/badlanguage.yaml", assert_exit_code=8)


@pytest.mark.kinda_slow
def test_nonexisting_file(run_semgrep_in_tmp: RunSemgrep, snapshot):
    run_semgrep_in_tmp("rules/does_not_exist.yaml", assert_exit_code=7)


@pytest.mark.kinda_slow
def test_rule_parser__empty(run_semgrep_in_tmp: RunSemgrep, snapshot):
    run_semgrep_in_tmp(f"rules/syntax/empty.yaml", assert_exit_code=7)


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filename", syntax_fails)
@pytest.mark.osemfail
def test_rule_parser__failure__error_messages(
    run_semgrep_in_tmp: RunSemgrep, snapshot, filename
):
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
@pytest.mark.osemfail
def test_rule_parser_cli_pattern(run_semgrep_in_tmp: RunSemgrep, snapshot):
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


# This test should ideally pass with osemgrep, but a key name error is a
# Rule.invalid_rule_error, which is classified as "recoverable"
# This means that it goes through a different code path than the unrecoverable
# errors below.
# Basically, what we should want to do is properly raise the `Exit_code.missing_config`
# with a descriptive error, or validate the rule according to JSON schema as a
# precursor step, before we go on to try and run the rules.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_rule_parser_error_key_name_text(run_semgrep_in_tmp: RunSemgrep, snapshot):
    # Check pretty print output
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/invalid-key-name.yml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=7,
    )
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.kinda_slow
def test_rule_parser_error_metavariable_text(run_semgrep_in_tmp: RunSemgrep, snapshot):
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/invalid-metavariable-regex.yml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=7,
    )

    assert "invalid configuration file found" in stderr


@pytest.mark.kinda_slow
def test_rule_parser_error_invalid_key_name_text(run_semgrep_in_tmp: RunSemgrep):
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/invalid-patterns-key.yml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=7,
    )

    assert "invalid configuration file found" in stderr


@pytest.mark.kinda_slow
def test_rule_parser_semgrep_test_invalid_rule(run_semgrep_in_tmp: RunSemgrep):
    _, stderr = run_semgrep_in_tmp(
        f"rules/syntax/invalid-patterns-key.yml",
        subcommand="test",
        target_name="targets/basic/basic.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
        assert_exit_code=7,
    )

    assert "invalid configuration" in stderr
