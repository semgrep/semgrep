def test_debugging_json(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="basic/stupid.py",
            output_format="debugging-json",
        ),
        "results.json",
    )
