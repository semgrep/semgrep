def test_paths(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/paths.yaml", target_name="exclude_include")[0],
        "results.json",
    )
