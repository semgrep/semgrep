#
# Copyright (c) 2023-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import json
from pathlib import Path

import pytest
from tests.conftest import skip_on_windows
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
        # Check that the SARIF output contains the URL from
        # metadata.source or metadata.source-rule-url:
        ("rules/source_url.yaml", "basic/stupid.py"),
        ("rules/source_rule_url.yaml", "basic/stupid.py"),
    ],
    ids=[
        "eqeq",
        "cwe_tag",
        "metavariable_type",
        "source_url",
        "source_rule_url",
    ],
)
@pytest.mark.parametrize("dataflow_traces", [True, False], ids=["trace", "notrace"])
@skip_on_windows  # matchBasedId change
def test_sarif_output(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule_and_target, dataflow_traces
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
    posix_snapshot.assert_match(res.stdout, "results.sarif")


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
@skip_on_windows  # matchBasedId change, better backslash replace logic
def test_sarif_output_osemfail(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule_and_target, dataflow_traces
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
    posix_snapshot.assert_match(res.stdout, "results.sarif")


# If there are nosemgrep comments to ignore findings, SARIF output should
# include them labeled as suppressed.
@pytest.mark.kinda_slow
@skip_on_windows  # matchBasedId change
def test_sarif_output_include_nosemgrep(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/regex/regex-nosemgrep.yaml",
            target_name="basic/regex-nosemgrep.txt",
            output_format=OutputFormat.SARIF,
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )


# Regression for https://github.com/semgrep/semgrep/issues/11715:
# when --sarif-output is used alongside the default terminal text output,
# nosemgrep-suppressed findings must not appear in the terminal text or
# inflate the summary count. The SARIF-side "suppressed matches preserved
# with a `suppressions` attribute" half of the contract is covered by
# test_sarif_output_include_nosemgrep above.
@pytest.mark.kinda_slow
@skip_on_windows
def test_text_terminal_filters_nosemgrep_with_sarif_output(
    run_semgrep_in_tmp: RunSemgrep, tmp_path: Path
):
    sarif_path = tmp_path / "out.sarif"
    result = run_semgrep_in_tmp(
        "rules/regex/regex-nosemgrep.yaml",
        target_name="basic/regex-nosemgrep.txt",
        # No --sarif/--json flag: leave the default terminal text output.
        output_format=None,
        options=["--sarif-output", str(sarif_path)],
        is_logged_in_weak=True,
    )

    # The target has two matches: the suppressed line ends with `# nosemgrep`,
    # the non-suppressed line does not. Only the non-suppressed one should
    # appear in the terminal text output.
    assert "aws_account_id:123456789012" in result.stdout, result.stdout
    assert "# nosemgrep" not in result.stdout, result.stdout
    # The scan-summary line (stderr) should also count one finding, not two.
    # Use the trailing "Ran ... finding" line because it appears in both
    # pysemgrep and osemgrep summary formats.
    assert "Ran 1 rule on 1 file: 1 finding." in result.stderr, result.stderr


# Pins the per-formatter contract introduced by the engine-1824 fix: any
# concurrent non-SARIF file output strips suppressed matches while SARIF
# retains them. Without this test, a future regression where the JSON
# formatter (or any other non-SARIF formatter) silently keeps suppressed
# matches would not be caught by the text-only test above.
# osemfail: osemgrep does not write --json-output files (see
# test_additional_outputs_with_format_flag in test_output.py).
@pytest.mark.kinda_slow
@pytest.mark.osemfail
@skip_on_windows
def test_sarif_output_with_json_output_filters_nosemgrep_from_json(
    run_semgrep_in_tmp: RunSemgrep, tmp_path: Path
):
    sarif_path = tmp_path / "out.sarif"
    json_path = tmp_path / "out.json"
    run_semgrep_in_tmp(
        "rules/regex/regex-nosemgrep.yaml",
        target_name="basic/regex-nosemgrep.txt",
        output_format=None,
        options=[
            "--sarif-output",
            str(sarif_path),
            "--json-output",
            str(json_path),
        ],
        is_logged_in_weak=True,
    )

    json_out = json.loads(json_path.read_text())
    assert len(json_out["results"]) == 1, json_out["results"]

    sarif = json.loads(sarif_path.read_text())
    results = sarif["runs"][0]["results"]
    assert len(results) == 2, results
    suppressed = [r for r in results if r.get("suppressions")]
    assert len(suppressed) == 1, results


# Pins the load-bearing `disable_nosem` branch in OutputHandler: when the
# user explicitly passes --disable-nosem, suppressed matches must reach
# the terminal text output and count toward the summary, even when a
# concurrent --sarif-output keeps them upstream.
@pytest.mark.kinda_slow
@skip_on_windows
def test_disable_nosem_includes_suppressed_in_text_when_sarif_output_present(
    run_semgrep_in_tmp: RunSemgrep, tmp_path: Path
):
    sarif_path = tmp_path / "out.sarif"
    result = run_semgrep_in_tmp(
        "rules/regex/regex-nosemgrep.yaml",
        target_name="basic/regex-nosemgrep.txt",
        output_format=None,
        options=["--disable-nosem", "--sarif-output", str(sarif_path)],
        is_logged_in_weak=True,
    )

    # Both matches should reach the terminal text output, including the
    # one annotated with `# nosemgrep`.
    assert "aws_account_id = 123456789012" in result.stdout, result.stdout
    assert "aws_account_id:123456789012" in result.stdout, result.stdout
    # And the summary should count both. Use the trailing "Ran ... findings"
    # line because it appears in both pysemgrep and osemgrep summary formats.
    assert "Ran 1 rule on 1 file: 2 findings." in result.stderr, result.stderr


# Test that rule board information makes its way into SARIF output
@pytest.mark.kinda_slow
@skip_on_windows  # matchBasedId change
def test_sarif_output_rule_board(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/rule-board-eqeq.yaml",
            target_name="basic/stupid.py",
            output_format=OutputFormat.SARIF,
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )


@pytest.mark.kinda_slow
@skip_on_windows  # matchBasedId change
def test_sarif_output_with_source(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    stdout = run_semgrep_in_tmp(
        "rules/eqeq-source.yml",
        env={"MOCK_USING_REGISTRY": "1"},
        output_format=OutputFormat.SARIF,
        is_logged_in_weak=True,
    ).stdout

    posix_snapshot.assert_match(
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
@skip_on_windows  # matchBasedId change
def test_sarif_output_with_source_edit(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    stdout = run_semgrep_in_tmp(
        "rules/eqeq-meta.yaml",
        output_format=OutputFormat.SARIF,
        is_logged_in_weak=True,
    ).stdout

    posix_snapshot.assert_match(stdout, "results.sarif")

    # Assert that each sarif rule object has a helpURI
    for rule in json.loads(stdout)["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("help", None) is not None


@pytest.mark.kinda_slow
@pytest.mark.osemfail
@skip_on_windows  # matchBasedId change
def test_sarif_output_with_nosemgrep_and_error(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
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
@skip_on_windows  # matchBasedId change
def test_sarif_output_with_autofix(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
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
@skip_on_windows  # matchBasedId change, better backslash replace logic
def test_sarif_output_with_dataflow_traces(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
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
@skip_on_windows  # better backslash replace logic
def test_sarif_output_when_errors(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="basic/inexistent.py",
            output_format=OutputFormat.SARIF,
            assert_exit_code=2,
            is_logged_in_weak=True,
        ).stdout,
        "results.sarif",
    )
