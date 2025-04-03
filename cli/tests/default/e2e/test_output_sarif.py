import json

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule_and_target",
    [
        # Simple case that should pass.
        ("rules/eqeq.yaml", "basic/stupid.py"),
        # Whenever there's a CWE tag, there should be a security tag.
        ("rules/cwe_tag.yaml", "basic/stupid.py"),
        # Rules with metavariable-type need parser initialization to parse correctly.
        ("rules/metavariable_type.yaml", "basic/stupid.py"),
    ],
)
@pytest.mark.parametrize("dataflow_traces", [True, False])
def test_sarif_output(
    run_semgrep_in_tmp: RunSemgrep, snapshot, rule_and_target, dataflow_traces
):
    rule, target = rule_and_target
    if dataflow_traces:
        options = ["--verbose", "--dataflow-traces"]
    else:
        options = ["--verbose"]

    res = run_semgrep_in_tmp(
        rule,
        target_name=target,
        options=options,
        output_format=OutputFormat.SARIF,
        assert_exit_code=0,
        is_logged_in_weak=True,
    )
    snapshot.assert_match(res.stdout, "results.sarif")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule_and_target",
    [
        # TODO: osemgrep does not take into account labels
        # and the rule.py formula_string() is not fully ported
        ("rules/taint_trace.yaml", "taint/taint_trace.cpp"),
    ],
)
@pytest.mark.parametrize("dataflow_traces", [True, False])
@pytest.mark.osemfail
def test_sarif_output_osemfail(
    run_semgrep_in_tmp: RunSemgrep, snapshot, rule_and_target, dataflow_traces
):
    rule, target = rule_and_target
    if dataflow_traces:
        options = ["--verbose", "--dataflow-traces"]
    else:
        options = ["--verbose"]

    res = run_semgrep_in_tmp(
        rule,
        target_name=target,
        options=options,
        output_format=OutputFormat.SARIF,
        assert_exit_code=0,
        is_logged_in_weak=True,
    )
    snapshot.assert_match(res.stdout, "results.sarif")


# If there are nosemgrep comments to ignore findings, SARIF output should
# include them labeled as suppressed.
@pytest.mark.kinda_slow
def test_sarif_output_include_nosemgrep(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-nosemgrep.yaml",
            target_name="basic/regex-nosemgrep.txt",
            output_format=OutputFormat.SARIF,
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )


# Test that rule board information makes its way into SARIF output
@pytest.mark.kinda_slow
def test_sarif_output_rule_board(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/rule-board-eqeq.yaml",
            target_name="basic/stupid.py",
            output_format=OutputFormat.SARIF,
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
def test_sarif_output_with_source(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout = run_semgrep_in_tmp(
        "rules/eqeq-source.yml",
        env={"MOCK_USING_REGISTRY": "1"},
        output_format=OutputFormat.SARIF,
        is_logged_in_weak=True,
    ).stdout

    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq-source.yml",
            output_format=OutputFormat.SARIF,
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )

    rules = json.loads(stdout)["runs"][0]["tool"]["driver"]["rules"]
    # Assert that each sarif rule object has a helpURI
    for rule in rules:
        assert rule.get("helpUri", None) is not None

    # Assert that we have our awareness nudge for our pro product
    # TODO: you need to be logged in now to get rules so we get
    # a bigger nudge now
    # assert "sg.run/pro" in rules[0].get("help", {}).get("text") or ""


@pytest.mark.kinda_slow
def test_sarif_output_with_source_edit(run_semgrep_in_tmp: RunSemgrep, snapshot):
    stdout = run_semgrep_in_tmp(
        "rules/eqeq-meta.yaml",
        output_format=OutputFormat.SARIF,
        is_logged_in_weak=True,
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
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
def test_sarif_output_with_autofix(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/autofix/autofix.yaml",
            target_name="autofix/autofix.py",
            output_format=OutputFormat.SARIF,
            options=["--autofix", "--dryrun"],
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
def test_sarif_output_with_dataflow_traces(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/taint.yaml",
            target_name="taint/taint.py",
            output_format=OutputFormat.SARIF,
            options=["--dataflow-traces"],
            is_logged_in_weak=True,
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
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )
