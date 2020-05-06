def test_equivalence(run_semgrep, snapshot):
    snapshot.assert_match(
        run_semgrep("rules/equivalence.yaml", target_name="equivalence"),
        "results.json",
    )
