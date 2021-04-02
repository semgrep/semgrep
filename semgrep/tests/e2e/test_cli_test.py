def test_cli_test_basic(run_semgrep_in_tmp, snapshot):
    results = run_semgrep_in_tmp(
        "rules/cli_test/basic/",
        options=["--test"],
        target_name="cli_test/basic/",
        output_format="json",
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
        output_format="json",
    )
    snapshot.assert_match(
        results,
        "results.json",
    )
