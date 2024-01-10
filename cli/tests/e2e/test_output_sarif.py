import json

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# If there are nosemgrep comments to ignore findings, SARIF output should include them
# labeled as suppressed.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_include_nosemgrep(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex-nosemgrep.yaml",
            target_name="basic/regex-nosemgrep.txt",
            output_format=OutputFormat.SARIF,
        ).stdout,
        "results.sarif",
    )


# Test that rule board information makes its way into SARIF output
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_rule_board(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/rule-board-eqeq.yaml",
            target_name="basic/stupid.py",
            output_format=OutputFormat.SARIF,
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_with_source(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout = run_semgrep_in_tmp(
        "rules/eqeq-source.yml",
        env={"MOCK_USING_REGISTRY": "1"},
        output_format=OutputFormat.SARIF,
    ).stdout

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-source.yml", output_format=OutputFormat.SARIF
        ).stdout,
        "results.sarif",
    )

    rules = json.loads(stdout)["runs"][0]["tool"]["driver"]["rules"]
    # Assert that each sarif rule object has a helpURI
    for rule in rules:
        assert rule.get("helpUri", None) is not None

    # Assert that we have our awareness nudge for our pro product
    assert "sg.run/pro" in rules[0].get("help", {}).get("text") or ""


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_with_source_edit(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout = run_semgrep_in_tmp(
        "rules/eqeq-meta.yaml", output_format=OutputFormat.SARIF
    ).stdout

    snapshot.assert_match(stdout, "results.sarif")

    # Assert that each sarif rule object has a helpURI
    for rule in json.loads(stdout)["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("help", None) is not None


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_with_nosemgrep_and_error(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="nosemgrep/eqeq-nosemgrep.py",
            output_format=OutputFormat.SARIF,
            options=["--error"],
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_with_autofix(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/autofix/autofix.yaml",
            target_name="autofix/autofix.py",
            output_format=OutputFormat.SARIF,
            options=["--autofix", "--dryrun"],
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_with_dataflow_traces(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint.yaml",
            target_name="taint/taint.py",
            output_format=OutputFormat.SARIF,
            options=["--dataflow-traces"],
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sarif_output_when_errors(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="basic/inexistent.py",
            output_format=OutputFormat.SARIF,
            assert_exit_code=2,
        ).stdout,
        "results.sarif",
    )
