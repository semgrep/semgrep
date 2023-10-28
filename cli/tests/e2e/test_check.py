import json
import os
import subprocess
import tempfile
from pathlib import Path

import pytest
from tests.conftest import _clean_stdout
from tests.conftest import mask_variable_text
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SEMGREP_BASE_COMMAND
from tests.semgrep_runner import SEMGREP_BASE_COMMAND_STR

from semgrep.constants import OutputFormat


def _mask_times(result_json: str) -> str:
    result = json.loads(result_json)

    def zero_times(value):
        if type(value) == float:
            return 2.022
        elif type(value) == list:
            return [zero_times(val) for val in value]
        elif type(value) == dict:
            return {k: zero_times(v) for k, v in value.items()}
        else:
            return value

    if "time" in result:
        result["time"] = zero_times(result["time"])
    return json.dumps(result, indent=2, sort_keys=True)


GITHUB_TEST_GIST_URL = (
    "https://raw.githubusercontent.com/returntocorp/semgrep-rules/develop/template.yaml"
)


@pytest.mark.kinda_slow
def test_basic_rule__local(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/eqeq.yaml").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_basic_rule__relative(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/../rules/eqeq.yaml").stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_basic_jsonnet_rule(
    monkeypatch: pytest.MonkeyPatch, run_semgrep_in_tmp: RunSemgrep, snapshot
):
    monkeypatch.setenv("R2C_INTERNAL_JSONNET_LIB", "rules/jsonnet/lib")
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/jsonnet/python/basic.jsonnet").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_deduplication(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Check that semgrep runs a rule only once even when different in the metadata
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/duplicate-rule.yaml", target_name="basic/stupid.py"
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_noextension_filtering(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Check that semgrep does not filter out files without extensions when
    said file is explicitly passed AND when we use --scan-unknown-extensions.
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml",
            target_name="basic/stupid_no_extension",
            options=["--scan-unknown-extensions"],
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_noextension_filtering_optimizations(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Check that semgrep does not filter out files without extensions when
    said file is explicitly passed AND when we use --scan-unknown-extensions.
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml",
            target_name="basic/stupid_no_extension",
            options=["--scan-unknown-extensions", "--optimizations", "all"],
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_script(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Validates that Semgrep scans scripts with matching shebangs
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-python.yaml",
            target_name="script/",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_extract(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Validates that Semgrep works with extract mode
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/extract_rules/js_html_concat.yaml",
            target_name="extract/js_html_concat.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_extract_exclude(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Validates that Semgrep works with extract mode
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/extract_rules/python_jupyter_paths_exclude.yaml",
            target_name="extract/python_jupyter_paths_exclude.ipynb",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_extract_include(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Validates that Semgrep works with extract mode
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/extract_rules/python_jupyter_paths_include.yaml",
            target_name="extract/python_jupyter_paths_include.ipynb",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_basic_rule__absolute(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(Path.cwd() / "rules" / "eqeq.yaml").stdout,
        "results.json",
    )


@pytest.mark.slow
def test_terminal_output(run_semgrep_in_tmp: RunSemgrep, snapshot):
    # Have shared settings file to test second run doesnt show metric output
    settings_file = tempfile.NamedTemporaryFile().name

    results = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        env={"SEMGREP_SETTINGS_FILE": settings_file},
    )
    snapshot.assert_match(results.as_snapshot(), "results.txt")

    # Metric message should not appear in second output
    results = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        env={"SEMGREP_SETTINGS_FILE": settings_file},
    )
    snapshot.assert_match(results.as_snapshot(), "results_second.txt")


@pytest.mark.kinda_slow
def test_terminal_output_quiet(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Quiet output should just have finding output
    """
    results = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        output_format=OutputFormat.TEXT,
        force_color=True,
        quiet=True,
        # Pass named temporary file to force metric notice behavior on first scan
        # (but should not see anything cause of --quiet)
        env={"SEMGREP_SETTINGS_FILE": tempfile.NamedTemporaryFile().name},
    )
    snapshot.assert_match(results.as_snapshot(), "results.txt")


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_stdin_input(snapshot):
    unique_settings_file = tempfile.NamedTemporaryFile().name
    Path(unique_settings_file).write_text(
        "anonymous_user_id: 5f52484c-3f82-4779-9353-b29bbd3193b6\n"
        "has_shown_metrics_notification: true\n"
    )
    process = subprocess.Popen(
        SEMGREP_BASE_COMMAND + ["--json", "-e", "a", "--lang", "js", "-"],
        encoding="utf-8",
        env={
            **os.environ,
            "SEMGREP_SETTINGS_FILE": unique_settings_file,
            "SEMGREP_VERSION_CACHE_PATH": tempfile.TemporaryDirectory().name,
            "SEMGREP_ENABLE_VERSION_CHECK": "0",
            "SEMGREP_SEND_METRICS": "off",
        },
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    stdout, _ = process.communicate("a")
    snapshot.assert_match(mask_variable_text(stdout), "results.json")


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_subshell_input(snapshot):
    unique_settings_file = tempfile.NamedTemporaryFile().name
    Path(unique_settings_file).write_text(
        "anonymous_user_id: 5f52484c-3f82-4779-9353-b29bbd3193b6\n"
        "has_shown_metrics_notification: true\n"
    )
    stdout = subprocess.check_output(
        [
            "bash",
            "-c",
            f"{SEMGREP_BASE_COMMAND_STR} --json -e 'a' --lang js <(echo 'a')",
        ],
        encoding="utf-8",
        env={
            **os.environ,
            "SEMGREP_SETTINGS_FILE": unique_settings_file,
            "SEMGREP_VERSION_CACHE_PATH": tempfile.TemporaryDirectory().name,
            "SEMGREP_ENABLE_VERSION_CHECK": "0",
            "SEMGREP_SEND_METRICS": "off",
        },
    )
    # Clean fingerprint from result since it's path dependent and that changes
    # everytime due to the way stdin works
    snapshot.assert_match(mask_variable_text(stdout), "results.json")


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_multi_subshell_input(snapshot):
    unique_settings_file = tempfile.NamedTemporaryFile().name
    Path(unique_settings_file).write_text(
        "anonymous_user_id: 5f52484c-3f82-4779-9353-b29bbd3193b6\n"
        "has_shown_metrics_notification: true\n"
    )
    stdout = subprocess.check_output(
        [
            "bash",
            "-c",
            f"{SEMGREP_BASE_COMMAND_STR} --json -e 'a' --lang js <(echo 'a') <(echo 'b + a')",
        ],
        encoding="utf-8",
        env={
            **os.environ,
            "SEMGREP_SETTINGS_FILE": unique_settings_file,
            "SEMGREP_VERSION_CACHE_PATH": tempfile.TemporaryDirectory().name,
            "SEMGREP_ENABLE_VERSION_CHECK": "0",
            "SEMGREP_SEND_METRICS": "off",
        },
    )
    snapshot.assert_match(mask_variable_text(stdout), "results.json")


@pytest.mark.kinda_slow
def test_multiline(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/multiline.yaml", target_name="multiline").stdout,
        "results.json",
    )


@pytest.mark.slow
def test_url_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(GITHUB_TEST_GIST_URL).stdout, "results.json"
    )


@pytest.mark.slow
def test_auto_config(run_semgrep_in_tmp: RunSemgrep, mocker):
    # --config auto will change over time, so lets just make sure this doesn't error out
    # TODO: Mock config response for more detailed testing
    # Use --no-strict to avoid error from unmatched nosem comment
    mocker.patch("semgrep.metrics.Metrics.send")
    run_semgrep_in_tmp(
        "auto", target_name="auto", force_metrics_off=False, strict=False
    )
    assert True


@pytest.mark.kinda_slow
def test_hidden_rule__explicit(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/hidden/.hidden").stdout, "results.json"
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_hidden_rule__implicit(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, _ = run_semgrep_in_tmp("rules/hidden", assert_exit_code=7)
    snapshot.assert_match(_clean_stdout(stdout), "error.json")

    _, stderr = run_semgrep_in_tmp(
        "rules/hidden", output_format=OutputFormat.TEXT, assert_exit_code=7
    )
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.kinda_slow
def test_regex_rule__top(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex-top.yaml").stdout, "results.json"
    )


@pytest.mark.kinda_slow
def test_regex_rule__utf8(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-utf8.yaml", target_name="basic/regex-utf8.txt"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__utf8_on_image(run_semgrep_in_tmp: RunSemgrep, snapshot):
    # https://github.com/returntocorp/semgrep/issues/4258
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-utf8.yaml", target_name="image/semgrep.png"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__child(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/regex-child.yaml").stdout, "results.json"
    )


@pytest.mark.kinda_slow
def test_regex_rule__not(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not.yaml", target_name="basic/stupid.py"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__not2(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not2.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__pattern_regex_and_pattern_not_regex(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/regex-not-with-pattern-regex.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_rule__issue2465(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-not-regex/issue2465.yaml",
            target_name="pattern-not-regex/issue2465.requirements.txt",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_regex_rule__invalid_expression(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp("rules/regex-invalid.yaml", assert_exit_code=2)
    snapshot.assert_match(stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(stdout), "error.json")


@pytest.mark.kinda_slow
def test_nested_patterns_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-patterns.yaml").stdout, "results.json"
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_nested_pattern_either_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-pattern-either.yaml").stdout, "results.json"
    )


# TODO: This can be unmarked osemfail once we port cli_unique_key deduplication
# https://github.com/returntocorp/semgrep/pull/8510
@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_metavariable_regex_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex.yaml").stdout, "results.json"
    )


@pytest.mark.kinda_slow
def test_metavariable_regex_multi_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-rule.yaml").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_multi_regex_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-regex-multi-regex.yaml").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_with_any_language_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language.yaml", target_name="basic/regex-any-language.html"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_with_any_language_multiple_rule(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-multiple.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_invalid_regex_with_any_language_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/regex-any-language-invalid.yaml",
        target_name="basic/regex-any-language.html",
        assert_exit_code=7,
    )
    snapshot.assert_match(stderr, "error.txt")
    snapshot.assert_match(_clean_stdout(stdout), "error.json")


@pytest.mark.kinda_slow
def test_regex_with_any_language_rule_none_alias(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_regex_with_any_language_multiple_rule_none_alias(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-any-language-multiple-alias-none.yaml",
            target_name="basic/regex-any-language.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.slow
def test_timeout(run_semgrep_in_tmp: RunSemgrep, snapshot):
    # Check that semgrep-core timeouts are properly handled

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/long.yaml",
            options=["--timeout", "1"],
            target_name="equivalence",
            strict=False,
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.slow
def test_spacegrep_timeout(run_semgrep_in_tmp: RunSemgrep, snapshot):
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
        options=["--lang=generic", "--pattern", pattern, "--timeout=1"],
        output_format=OutputFormat.TEXT,
        strict=False,  # don't fail due to timeout
    )

    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_max_memory(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/long.yaml",
        options=["--verbose", "--max-memory", "1"],
        target_name="equivalence",
        strict=False,
    )
    snapshot.assert_match(_mask_times(stdout), "results.json")
    snapshot.assert_match(stderr, "error.txt")


@pytest.mark.osemfail
@pytest.mark.slow
def test_stack_size(run_semgrep_in_tmp: RunSemgrep, snapshot):
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
        f"ulimit -s 1000 && {SEMGREP_BASE_COMMAND_STR} --disable-version-check --metrics off --config {rulepath} --verbose {targetpath}",
        shell=True,
        capture_output=True,
        encoding="utf-8",
    )
    print(output.stderr)
    assert (
        "semgrep-core exit code: -11" in output.stderr
        or "Stack overflow" in output.stderr
    )

    # If only set soft limit, semgrep should raise it as necessary so we don't hit soft limit
    output = subprocess.run(
        f"ulimit -S -s 1000 && {SEMGREP_BASE_COMMAND_STR} --disable-version-check --metrics off --config {rulepath} --verbose {targetpath}",
        shell=True,
        capture_output=True,
        encoding="utf-8",
    )
    # with a soft limit, semgrep should terminate without errors
    assert "semgrep-core exit code: -11" not in output.stderr
    assert "Stack overflow" not in output.stderr


@pytest.mark.osemfail
@pytest.mark.slow
def test_timeout_threshold(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results = run_semgrep_in_tmp(
        "rules/multiple-long.yaml",
        options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
        target_name="equivalence",
        strict=False,
    ).stdout
    snapshot.assert_match(
        _mask_times(results),
        "results.json",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            force_color=True,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "1"],
            target_name="equivalence",
            strict=False,
        ).stderr,
        "error.txt",
    )

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multiple-long.yaml",
            output_format=OutputFormat.TEXT,
            force_color=True,
            options=["--verbose", "--timeout", "1", "--timeout-threshold", "2"],
            target_name="equivalence",
            strict=False,
        ).stderr,
        "error_2.txt",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison.yaml").stdout, "results.json"
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_base(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-base.yaml").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_strip(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-strip.yaml").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_bad_content(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/metavariable-comparison-bad-content.yaml").stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_multiple_configs_file(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", "rules/eqeq-python.yaml"]).stdout,
        "results.json",
    )


@pytest.mark.slow
def test_multiple_configs_different_origins(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(["rules/eqeq.yaml", GITHUB_TEST_GIST_URL]).stdout,
        "results.json",
    )


# TODO: This can be unmarked osemfail once we port cli_unique_key deduplication
# https://github.com/returntocorp/semgrep/pull/8510
@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_metavariable_propagation_regex(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-regex-propagation.yaml",
            target_name="metavariable_propagation/metavariable-regex-propagation.py",
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_propagation_comparison(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-comparison-propagation.yaml",
            target_name="metavariable_propagation/metavariable-comparison-propagation.py",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_taint_mode(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint.yaml",
            target_name="taint/taint.py",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_deduplication_same_message(run_semgrep_in_tmp: RunSemgrep, snapshot):
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


@pytest.mark.kinda_slow
def test_deduplication_different_message(run_semgrep_in_tmp: RunSemgrep, snapshot):
    output, _ = run_semgrep_in_tmp(
        "rules/deduplication/duplication-different-message.yaml",
        target_name="deduplication/deduplication.py",
    )
    snapshot.assert_match(output, "results.json")
    json_output = json.loads(output)
    assert len(json_output["results"]) == 2


@pytest.mark.kinda_slow
def test_pattern_regex_empty_file(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/pattern-regex-empty-file.yaml",
            target_name="empty/totally_empty_file",
        ).stdout,
        "results.json",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_inventory_finding_output(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/inventory-rule.yaml",
            target_name="auto/fingerprints",
            strict=False,
            output_format=OutputFormat.TEXT,
        ).stderr,
        "output.txt",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_experiment_finding_output(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/experiment-rule.yaml",
            target_name="auto/fingerprints",
            strict=False,
            output_format=OutputFormat.TEXT,
        ).stderr,
        "output.txt",
    )


@pytest.mark.osemfail
@pytest.mark.quick
def multi_focus_metavariable(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/multi-focus-metavariable.yaml",
            target_name="targets/multi-focus-metavariable.py",
            strict=False,
            output_format=OutputFormat.TEXT,
        ).stderr,
        "output.txt",
    )


# Ensure that a rule restricted to a specific language [js] will not run
# on a target file in another language.
# The JavaScript rule should match only the JavaScript file and the Python
# rule should match only the Python file.
@pytest.mark.kinda_slow
def test_language_filtering(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/language-filtering.yaml",
            target_name="language-filtering",
        ).stdout,
        "results.json",
    )


# A simple test to check that per-rule include/exclude filtering is
# taking place in semgrep-core and osemgrep.
@pytest.mark.kinda_slow
def test_per_rule_include(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/per-rule-include.yaml",
            target_name="per-rule-include",
        ).stdout,
        "results.json",
    )


# Check that pysemgrep and osemgrep sort the results identically.
# We don't really need it as a product feature but it's important to
# compare the output of osemgrep with the output of pysemgrep.
@pytest.mark.quick
def test_sort_json_findings(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/sort-findings.yaml",
            target_name="sort-findings",
            strict=False,
        ).stdout,
        "results.json",
    )


# Check that pysemgrep and osemgrep sort the results as intended
# when presenting them in text format.
@pytest.mark.quick
def test_sort_text_findings(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/sort-findings.yaml",
            target_name="sort-findings",
            strict=False,
            output_format=OutputFormat.TEXT,
        ).stdout,
        "output.txt",
    )
