import json
import os
import subprocess
import tempfile
from pathlib import Path

import pytest
from tests.conftest import mask_floats
from tests.conftest import mask_variable_text
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND
from tests.semgrep_runner import SEMGREP_BASE_SCAN_COMMAND_STR

from semgrep.constants import OutputFormat


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


# TODO: I don't understand why this pass
@pytest.mark.kinda_slow
def test_verbose(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/basic.yaml",
        options=["--verbose"],
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )

    snapshot.assert_match(
        mask_floats(results),
        "results.txt",
    )


@pytest.mark.osemfail
@pytest.mark.kinda_slow
def test_time(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/basic.yaml",
        options=["--time"],
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )

    snapshot.assert_match(
        mask_floats(results),
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_show_supported_languages(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/basic.yaml",
        options=["--show-supported-languages"],
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
    )

    snapshot.assert_match(
        results,
        "results.txt",
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


@pytest.mark.kinda_slow
def test_noextension_with_explicit_lang(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Check that we can scan all the target files on the command line when
    specifying the pattern with -e or -f.
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            None,  # no --config
            target_name="basic/simple_python_no_extension",
            options=["--scan-unknown-extensions", "--lang", "python", "-e", "hello"],
        ).stdout,
        "results.json",
    )


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
        # Pass named temporary file to force metric notice behavior
        # on first scan (but should not see anything cause of --quiet)
        env={"SEMGREP_SETTINGS_FILE": tempfile.NamedTemporaryFile().name},
    )
    snapshot.assert_match(results.as_snapshot(), "results.txt")


# Feed 'a' to semgrep's stdin and search for the pattern 'a', expecting
# one finding.
@pytest.mark.kinda_slow
def test_stdin_input(snapshot):
    unique_settings_file = tempfile.NamedTemporaryFile().name
    Path(unique_settings_file).write_text(
        "anonymous_user_id: 5f52484c-3f82-4779-9353-b29bbd3193b6\n"
        "has_shown_metrics_notification: true\n"
    )
    process = subprocess.Popen(
        SEMGREP_BASE_SCAN_COMMAND + ["--json", "-e", "a", "--lang", "js", "-"],
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
            f"{SEMGREP_BASE_SCAN_COMMAND_STR} --json -e 'a' --lang js <(echo 'a')",
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
            f"{SEMGREP_BASE_SCAN_COMMAND_STR} --json -e 'a' --lang js <(echo 'a') <(echo 'b + a')",
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


# we use to throw an error for such test because we would not explore
# dot files under the config, but we don't anymore to simplify things.
@pytest.mark.kinda_slow
def test_hidden_rule__implicit(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(run_semgrep_in_tmp("rules/hidden/").stdout, "results.json")


@pytest.mark.kinda_slow
def test_nested_patterns_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-patterns.yaml").stdout, "results.json"
    )


@pytest.mark.kinda_slow
def test_nested_pattern_either_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/nested-pattern-either.yaml").stdout, "results.json"
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


@pytest.mark.kinda_slow
def test_taint_mode(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint.yaml",
            target_name="taint/taint.py",
        ).stdout,
        "results.json",
    )


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
@pytest.mark.slow
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
@pytest.mark.slow
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


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_match_rules_same_message(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/two_rules_same_message.yaml",
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


# It should accept rules with the new "CRITICAL" severity
@pytest.mark.kinda_slow
def test_critical_severity(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/severity_critical.yaml",
            target_name="basic.py",
        ).stdout,
        "results.json",
    )
