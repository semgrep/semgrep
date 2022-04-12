import json
from subprocess import CalledProcessError

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
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")
    assert excinfo.value.returncode != 0
    snapshot.assert_match(str(excinfo.value.returncode), "returncode.txt")


@pytest.mark.kinda_slow
def test_regex_with_bad_language(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/badlanguage.yaml")
    assert excinfo.value.returncode != 0


@pytest.mark.kinda_slow
def test_rule_parser__empty(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(f"rules/syntax/empty.yaml")
    assert excinfo.value.returncode != 0
    snapshot.assert_match(str(excinfo.value.returncode), "returncode.txt")


@pytest.mark.kinda_slow
@pytest.mark.parametrize("filename", syntax_fails)
def test_rule_parser__failure__error_messages(run_semgrep_in_tmp, snapshot, filename):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(f"rules/syntax/{filename}.yaml")

    json_output = json.loads(excinfo.value.stdout)
    # Something went wrong, so there had better be an error, and we asked for JSON so there had better be some output...
    assert json_output["errors"] != []

    with pytest.raises(CalledProcessError) as excinfo_in_color:
        run_semgrep_in_tmp(
            f"rules/syntax/{filename}.yaml",
            options=["--force-color"],
            output_format=OutputFormat.TEXT,
            force_color=True,
        )

    snapshot.assert_match(
        json.dumps(json_output, indent=2, sort_keys=True), "error.json"
    )

    if excinfo_in_color.value.stderr != excinfo.value.stderr:
        snapshot.assert_match(excinfo_in_color.value.stderr, "error-in-color.txt")


# https://github.com/returntocorp/semgrep/issues/1095
@pytest.mark.kinda_slow
def test_rule_parser_cli_pattern(run_semgrep_in_tmp, snapshot):
    # Check json output
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(options=["-e", "#include<asdf><<>>><$X>", "-l", "c"])
    json_output = json.loads(excinfo.value.stdout)
    snapshot.assert_match(
        json.dumps(json_output, indent=2, sort_keys=True), "error.json"
    )

    # Check pretty print output
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            options=["-e", "#include<asdf><<>>><$X>", "-l", "c"],
            output_format=OutputFormat.TEXT,
            force_color=True,
        )
    snapshot.assert_match(excinfo.value.stderr, "error.txt")


@pytest.mark.kinda_slow
def test_rule_parser_error_key_name_text(run_semgrep_in_tmp, snapshot):
    # Check pretty print output
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            f"rules/syntax/invalid-key-name.yml",
            output_format=OutputFormat.TEXT,
            force_color=True,
        )
    snapshot.assert_match(excinfo.value.stderr, "error.txt")


@pytest.mark.kinda_slow
def test_rule_parser_error_metavariable_text(run_semgrep_in_tmp, snapshot):

    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            f"rules/syntax/invalid-metavariable-regex.yml",
            output_format=OutputFormat.TEXT,
            force_color=True,
        )
    snapshot.assert_match(excinfo.value.stderr, "error.txt")


@pytest.mark.kinda_slow
def test_rule_parser_error_invalid_key_name_text(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            f"rules/syntax/invalid-patterns-key.yml",
            output_format=OutputFormat.TEXT,
            force_color=True,
        )
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
