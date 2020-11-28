def test_severity_error(run_semgrep_in_tmp, snapshot):
    json_str = run_semgrep_in_tmp("rules/inside.yaml", options=["--severity", "ERROR"])
    assert json_str != ""
    assert '"severity": "INFO"' not in json_str
    assert '"severity": "WARNING"' not in json_str


def test_severity_info(run_semgrep_in_tmp, snapshot):
    # Shouldn't return errors or results, since inside.yaml has 'severity: ERROR'
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/inside.yaml", options=["--severity", "INFO"]),
        "results.json",
    )


def test_severity_warning(run_semgrep_in_tmp, snapshot):
    # Shouldn't return errors or results, since inside.yaml has 'severity: ERROR'
    snapshot.assert_match(
        run_semgrep_in_tmp("rules/inside.yaml", options=["--severity", "WARNING"]),
        "results.json",
    )


def test_severity_multiple(run_semgrep_in_tmp, snapshot):
    # Shouldn't return errors or results, since inside.yaml has 'severity: ERROR'
    # Differs from the two preceding tests in that we're testing adding multiple severity strings
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/inside.yaml", options=["--severity", "INFO", "--severity", "WARNING"]
        ),
        "results.json",
    )
