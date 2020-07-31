def test_equivalence(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/equivalence.yaml", target_name="equivalence"),
        "results.json",
    )
