import os
import re
import subprocess
from pathlib import Path

import pytest
from tests.fixtures import RunSemgrep
from tests.semgrep_runner import SEMGREP_BASE_COMMAND

from semgrep.constants import OutputFormat


FLOATS = re.compile("([0-9]+).([0-9]+)")


def _mask_floats(text_output: str) -> str:
    return re.sub(FLOATS, "x.xxx", text_output)


@pytest.mark.kinda_slow
def test_cli_test_basic(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        options=["--test"],
        target_name="cli_test/basic/",
        output_format=OutputFormat.JSON,
    )

    snapshot.assert_match(
        results,
        "results.json",
    )


@pytest.mark.osempass
@pytest.mark.kinda_slow
def test_cli_test_verbose(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        options=["--verbose"],
        target_name="cli_test/basic/",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )

    snapshot.assert_match(
        _mask_floats(results),
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_cli_test_time(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        options=["--time"],
        target_name="cli_test/basic/",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )

    snapshot.assert_match(
        _mask_floats(results),
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_timeout(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/error/",
        options=["--test"],
        target_name="cli_test/error/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_cli_test_yaml_language(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/language/",
        options=["--test"],
        target_name="cli_test/language/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


@pytest.mark.osempass
@pytest.mark.kinda_slow
def test_cli_test_show_supported_languages(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        options=["--show-supported-languages"],
        target_name="cli_test/basic/",
        output_format=OutputFormat.TEXT,
    )

    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_cli_test_suffixes(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/suffixes/",
        options=["--test"],
        target_name="cli_test/suffixes/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_cli_test_multiline_annotations(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/multiple_annotations/",
        options=["--test"],
        target_name="cli_test/multiple_annotations/",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_parse_errors(run_semgrep_in_tmp: RunSemgrep, snapshot):
    _results, errors = run_semgrep_in_tmp(
        "rules/cli_test/parse_errors/",
        options=["--verbose"],
        target_name="cli_test/parse_errors/invalid_javascript.js",
        output_format=OutputFormat.TEXT,
        force_color=True,
        strict=False,
    )
    snapshot.assert_match(
        errors,
        "errors.txt",
    )


@pytest.mark.slow
def test_cli_test_from_entrypoint(snapshot):
    env = {}
    env["PATH"] = os.environ.get("PATH", "")

    cmd = SEMGREP_BASE_COMMAND + [
        "--test",
        "--config",
        "rules/cli_test/multiple_annotations/multiple-annotations.yaml",
        "targets/cli_test/multiple_annotations/multiple-annotations-bad.py",
    ]
    result = subprocess.run(
        cmd,
        cwd=Path(__file__).parent,
        capture_output=True,
        encoding="utf-8",
        check=True,
        env=env,
        timeout=15,
    )
    snapshot.assert_match(result.stdout, "output.txt")


@pytest.mark.kinda_slow
def test_cli_test_match_rules_same_message(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/match_rules_same_message/rules.yml",
        target_name="cli_test/basic/",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
def test_cli_test_ignore_rule_paths(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/cli_test/ignore_rule_paths/",
        options=["--test"],
        target_name="cli_test/ignore_rule_paths/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )
