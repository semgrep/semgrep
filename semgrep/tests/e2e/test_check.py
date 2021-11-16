import json
import subprocess
import tempfile
from pathlib import Path
from subprocess import CalledProcessError

import pytest
from tests.conftest import _clean_output_json

from semgrep.constants import OutputFormat

GITHUB_TEST_GIST_URL = (
    "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"
)


def test_basic_rule__local(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/eqeq.yaml")[0], "results.json")


def test_basic_rule__relative(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/../rules/eqeq.yaml")[0],
        "results.json",
    )


def test_deduplication(run_semgrep_in_tmp, snapshot):
    """
    Check that semgrep runs a rule only once even when different in the metadata
    """
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/duplicate-rule.yaml", target_name="basic/stupid.py")[
            0
        ],
        "results.json",
    )


def test_noextension_filtering(run_semgrep_in_tmp, snapshot):
    """
    Check that semgrep does not filter out files without extensions when
    said file is explicitly passed
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml", target_name="basic/stupid_no_extension"
        )[0],
        "results.json",
    )


def test_noextension_filtering_optimizations(run_semgrep_in_tmp, snapshot):
    """
    Check that semgrep does not filter out files without extensions when
    said file is explicitly passed
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml",
            target_name="basic/stupid_no_extension",
            options=["--optimizations", "all"],
        )[0],
        "results.json",
    )


def test_script(run_semgrep_in_tmp, snapshot):
    """
    Validates that Semgrep scans scripts with matching shebangs
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml",
            target_name="script/",
        )[0],
        "results.json",
    )


def test_basic_rule__absolute(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(Path.cwd() / "rules" / "eqeq.yaml")[0],
        "results.json",
    )


def test_terminal_output(run_semgrep_in_tmp, snapshot):
    # Have shared settings file to test second run doesnt show metric output
    settings_file = tempfile.NamedTemporaryFile().name

    text_output = run_semgrep_in_tmp(
        "rules/eqeq.yaml", output_format=OutputFormat.TEXT, settings_file=settings_file
    )
    snapshot.assert_match(text_output[0], "output.txt")
    snapshot.assert_match(text_output[1], "error.txt")

    # Metric message should not appear in second output
    text_output = run_semgrep_in_tmp(
        "rules/eqeq.yaml", output_format=OutputFormat.TEXT, settings_file=settings_file
    )
    snapshot.assert_match(text_output[0], "output_second.txt")
    snapshot.assert_match(text_output[1], "error_second.txt")


def test_terminal_output_quiet(run_semgrep_in_tmp, snapshot):
    """
    Quiet output should just have finding output
    """
    text_output = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        output_format=OutputFormat.TEXT,
        quiet=True,
        # Pass named temporary file to force metric notice behavior on first scan
        # (but should not see anything cause of --quiet)
        settings_file=tempfile.NamedTemporaryFile().name,
    )
    snapshot.assert_match(text_output[0], "output.txt")
    snapshot.assert_match(text_output[1], "error.txt")


def test_stdin_input(snapshot):
    process = subprocess.Popen(
        [
            "python3",
            "-m",
            "semgrep",
            "--disable-version-check",
            "--metrics",
            "off",
            "--json",
            "-e",
            "a",
            "--lang",
            "js",
            "-",
        ],
        encoding="utf-8",
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    stdout, _ = process.communicate("a")
    snapshot.assert_match(_clean_output_json(stdout), "results.json")


def test_subshell_input(snapshot):
    stdout = subprocess.check_output(
        [
            "bash",
            "-c",
            "python3 -m semgrep --disable-version-check --metrics off --json -e 'a' --lang js <(echo 'a')",
        ],
        encoding="utf-8",
    )
    snapshot.assert_match(_clean_output_json(stdout), "results.json")


def test_multi_subshell_input(snapshot):
    stdout = subprocess.check_output(
        [
            "bash",
            "-c",
            "python3 -m semgrep --disable-version-check --metrics off --json -e 'a' --lang js <(echo 'a') <(echo 'b + a')",
        ],
        encoding="utf-8",
    )
    snapshot.assert_match(_clean_output_json(stdout), "results.json")


def test_multiline(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq.yaml", target_name="multiline")[0],
        "results.json",
    )


def test_url_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(GITHUB_TEST_GIST_URL)[0],
        "results.json",
    )


def test_registry_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("r2c")[0],
        "results.json",
    )


def test_auto_config(run_semgrep_in_tmp):
    # --config auto will change over time, so lets just make sure this doesn't error out
    # TODO: Mock config response for more detailed testing
    run_semgrep_in_tmp("auto")
    assert True


def test_hidden_rule__explicit(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/hidden/.hidden")[0], "results.json")


def test_hidden_rule__implicit(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/hidden")[0]
    assert excinfo.value.returncode == 7
    snapshot.assert_match(excinfo.value.stdout, "error.json")

    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/hidden", output_format=OutputFormat.TEXT)[0]
    assert excinfo.value.returncode == 7
    snapshot.assert_match(excinfo.value.stderr, "error.txt")


def test_default_rule__file(run_semgrep_in_tmp, snapshot):
    Path(".semgrep.yml").symlink_to(Path("rules/eqeq.yaml").resolve())
    snapshot.assert_match(run_semgrep_in_tmp()[0], "results.json")


def test_default_rule__folder(run_semgrep_in_tmp, snapshot):
    Path(".semgrep").mkdir()
    Path(".semgrep/.semgrep.yml").symlink_to(Path("rules/eqeq.yaml").resolve())

    snapshot.assert_match(run_semgrep_in_tmp()[0], "results.json")


def test_regex_rule__top(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/regex-top.yaml")[0], "results.json")


def test_regex_rule__utf8(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex-utf8.yaml", target_name="basic/regex-utf8.txt")[
            0
        ],
        "results.json",
    )


def test_regex_rule__utf8_on_image(run_semgrep_in_tmp, snapshot):
    # https://github.com/returntocorp/semgrep/issues/4258
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex-utf8.yaml", target_name="image/semgrep.png")[0],
        "results.json",
    )


def test_regex_rule__child(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex-child.yaml")[0], "results.json"
    )


def test_regex_rule__not(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not.yaml", target_name="basic/stupid.py"
        )[0],
        "results.json",
    )


def test_regex_rule__not2(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not2.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_regex_rule__pattern_regex_and_pattern_not_regex(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not-with-pattern-regex.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_regex_rule__issue2465(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/issue2465.yaml",
            target_name="pattern-not-regex/issue2465.requirements.txt",
        )[0],
        "results.json",
    )


def test_regex_rule__invalid_expression(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp("rules/regex-invalid.yaml")[0]
    assert excinfo.value.returncode == 2
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_nested_patterns_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-patterns.yaml")[0], "results.json"
    )


def test_nested_pattern_either_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-pattern-either.yaml")[0], "results.json"
    )


def test_metavariable_regex_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex.yaml")[0], "results.json"
    )


def test_metavariable_regex_multi_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-rule.yaml")[0],
        "results.json",
    )


def test_metavariable_multi_regex_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-regex.yaml")[0],
        "results.json",
    )


def test_regex_with_any_language_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language.yaml", target_name="basic/regex-any-language.html"
        )[0],
        "results.json",
    )


def test_regex_with_any_language_multiple_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-multiple.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_invalid_regex_with_any_language_rule(run_semgrep_in_tmp, snapshot):
    with pytest.raises(CalledProcessError) as excinfo:
        run_semgrep_in_tmp(
            "rules/regex-any-language-invalid.yaml",
            target_name="basic/regex-any-language.html",
        )
    assert excinfo.value.returncode not in (0, 1)
    snapshot.assert_match(excinfo.value.stderr, "error.txt")
    snapshot.assert_match(excinfo.value.stdout, "error.json")


def test_regex_with_any_language_rule_none_alias(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_regex_with_any_language_multiple_rule_none_alias(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-multiple-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        )[0],
        "results.json",
    )


def test_timeout(run_semgrep_in_tmp, snapshot):
    # Check that semgrep-core timeouts are properly handled

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            options=["--timeout", "1"],
            target_name="equivalence",
            strict=False,
        )[0],
        "results.json",
    )


def test_spacegrep_timeout(run_semgrep_in_tmp, snapshot):
    # Check that spacegrep timeouts are handled gracefully.
    #
    # The pattern is designed to defeat any optimization that would
    # prevent a timeout. Both the words 'Frob' and 'Yoyodyne' occur
    # once in the file but in a different order, preventing any match.
    #
    pattern = "$A ... $B ... $C ... Frob ... Yoyodyne"

    stdout, stderr = run_semgrep_in_tmp(
        config=None,
        target_name="spacegrep_timeout/gnu-lgplv2.txt",
        options=[
            "--lang",
            "generic",
            "--pattern",
            pattern,
            "--timeout",
            "1",
        ],
        output_format=OutputFormat.TEXT,
        strict=False,  # don't fail due to timeout
    )

    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")


def test_max_memory(run_semgrep_in_tmp, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/long.yaml",
        options=["--verbose", "--max-memory", "1"],
        target_name="equivalence",
        strict=False,
    )
    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")


def test_stack_size(run_semgrep_in_tmp, snapshot):
    """
    Verify that semgrep raises the soft stack limit if possible
    when calling semgrep core
    """

    # long.yaml and equivalence were chosen since they happen to cause
    # stack exhaustion
    e2e_dir = Path(__file__).parent
    targetpath = Path(e2e_dir / "targets").resolve() / "equivalence"
    rulepath = Path(e2e_dir / "rules").resolve() / "long.yaml"

    # Set the hard as well as the soft stack limit. This should force a stack
    # overflow. If this fails, the test is broken and needs to be fixed.
    # Do not just delete this assertion. It means the actual test below does
    # not accurately verify that we are solving the stack exhaustion
    output = subprocess.run(
        f"ulimit -s 1000 && semgrep --disable-version-check --metrics off --config {rulepath} --verbose {targetpath}",
        shell=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        encoding="utf-8",
    )
    print(output.stderr)
    assert (
        "semgrep-core exit code: -11" in output.stderr
        or "Stack overflow" in output.stderr
    )

    # If only set soft limit, semgrep should raise it as necessary so we don't hit soft limit
    output = subprocess.run(
        f"ulimit -S -s 1000 && semgrep --disable-version-check --metrics off --config {rulepath} --verbose {targetpath}",
        shell=True,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        encoding="utf-8",
    )
    assert "semgrep-core exit code: -11" not in output.stderr
    assert "Stack overflow" not in output.stderr


def test_timeout_threshold(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
        )[0],
        "results.json",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
        )[1],
        "error.txt",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "2"],
            target_name="equivalence",
            strict=False,
        )[1],
        "error_2.txt",
    )


def test_metavariable_comparison_rule(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison.yaml")[0], "results.json"
    )


def test_metavariable_comparison_rule_base(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-base.yaml")[0], "results.json"
    )


def test_metavariable_comparison_rule_strip(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-strip.yaml")[0],
        "results.json",
    )


def test_metavariable_comparison_rule_bad_content(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-bad-content.yaml")[0],
        "results.json",
    )


def test_multiple_configs_file(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", "rules/eqeq-python.yaml"])[0],
        "results.json",
    )


def test_multiple_configs_different_origins(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", GITHUB_TEST_GIST_URL])[0], "results.json"
    )


def test_metavariable_propagation_regex(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-regex-propagation.yaml",
            target_name="metavariable_propagation/metavariable-regex-propagation.py",
        )[0],
        "results.json",
    )


def test_metavariable_propagation_comparison(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-comparison-propagation.yaml",
            target_name="metavariable_propagation/metavariable-comparison-propagation.py",
        )[0],
        "results.json",
    )


def test_taint_mode(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint.yaml",
            target_name="taint/taint.py",
        )[0],
        "results.json",
    )


def test_deduplication_same_message(run_semgrep_in_tmp, snapshot):
    """
    With same message, should deduplicate and only have one finding
    """
    output, _ = run_semgrep_in_tmp(
        "rules/deduplication/duplication-same-message.yaml",
        target_name="deduplication/deduplication.py",
    )
    snapshot.assert_match(output, "results.json")
    json_output = json.loads(output)
    assert len(json_output["results"]) == 1


def test_deduplication_different_message(run_semgrep_in_tmp, snapshot):
    output, _ = run_semgrep_in_tmp(
        "rules/deduplication/duplication-different-message.yaml",
        target_name="deduplication/deduplication.py",
    )
    snapshot.assert_match(output, "results.json")
    json_output = json.loads(output)
    assert len(json_output["results"]) == 2


def test_pattern_regex_empty_file(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-regex-empty-file.yaml",
            target_name="empty/totally_empty_file",
        )[0],
        "results.json",
    )


def test_cdn_ruleset_resolution(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("p/ci")[0],
        "results.json",
    )
