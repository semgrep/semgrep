def test_autofix(run_semgrep, snapshot):
    snapshot.assert_match(
        run_semgrep("rules/autofix.yaml", target_name="autofix"), "results.json",
    )
