import re

import pytest
from tests.fixtures import RunSemgrep

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
