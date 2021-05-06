from semgrep.constants import OutputFormat


def test_cli_test_basic(run_semgrep_in_tmp, snapshot):
    results = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        options=["--test"],
        target_name="cli_test/basic/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


def test_cli_test_yaml_language(run_semgrep_in_tmp, snapshot):
    results = run_semgrep_in_tmp(
        "rules/cli_test/language/",
        options=["--test"],
        target_name="cli_test/language/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


def test_cli_test_suffixes(run_semgrep_in_tmp, snapshot):
    results = run_semgrep_in_tmp(
        "rules/cli_test/suffixes/",
        options=["--test"],
        target_name="cli_test/suffixes/",
        output_format=OutputFormat.JSON,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )


def test_cli_test_multiline_annotations(run_semgrep_in_tmp, snapshot):
    results = run_semgrep_in_tmp(
        "rules/cli_test/multiple_annotations/",
        options=["--test"],
        target_name="cli_test/multiple_annotations/",
        output_format=OutputFormat.TEXT,
    )
    snapshot.assert_match(
        results,
        "results.json",
    )
