def test_autofix(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/autofix.yaml", target_name="autofix"), "results.json",
    )
