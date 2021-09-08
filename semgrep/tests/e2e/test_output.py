import json

import pytest

from semgrep import __VERSION__
from semgrep.constants import OutputFormat


def _clean_sarif_output(output):
    # Rules are logically a set so the JSON list's order doesn't matter
    # we make the order deterministic here so that snapshots match across runs
    # the proper solution will be https://github.com/joseph-roitman/pytest-snapshot/issues/14
    output["runs"][0]["tool"]["driver"]["rules"] = sorted(
        output["runs"][0]["tool"]["driver"]["rules"],
        key=lambda rule: str(rule["id"]),
    )

    # Semgrep version is included in sarif output. Verify this independently so
    # snapshot does not need to be updated on version bump
    assert output["runs"][0]["tool"]["driver"]["semanticVersion"] == __VERSION__
    output["runs"][0]["tool"]["driver"]["semanticVersion"] = "placeholder"

    return output


@pytest.mark.parametrize("format", ["--sarif", "--junit-xml", "--emacs", "--vim"])
def test_output_format(run_semgrep_in_tmp, snapshot, format):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=[format],
        output_format=OutputFormat.TEXT,  # Not the real output format; just disables JSON parsing
    )
    snapshot.assert_match(stdout, "results.out")


# If there are nosemgrep comments to ignore findings, SARIF output should include them
# labeled as suppressed.
def test_sarif_output_include_nosemgrep(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp(
            "rules/regex-nosemgrep.yaml",
            target_name="basic/regex-nosemgrep.txt",
            output_format=OutputFormat.SARIF,
        )[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )


def test_sarif_output_with_source(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq-source.yml", output_format=OutputFormat.SARIF)[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )

    # Assert that each sarif rule object has a helpURI
    for rule in sarif_output["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("helpUri", None) is not None


def test_sarif_output_with_source_edit(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp("rules/eqeq-meta.yaml", output_format=OutputFormat.SARIF)[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )

    # Assert that each sarif rule object has a helpURI
    for rule in sarif_output["runs"][0]["tool"]["driver"]["rules"]:
        assert rule.get("help", None) is not None


def test_sarif_output_with_nosemgrep_and_error(run_semgrep_in_tmp, snapshot):
    sarif_output = json.loads(
        run_semgrep_in_tmp(
            "rules/eqeq.yaml",
            target_name="nosemgrep/eqeq-nosemgrep.py",
            output_format=OutputFormat.SARIF,
            options=["--error"],
        )[0]
    )

    sarif_output = _clean_sarif_output(sarif_output)

    snapshot.assert_match(
        json.dumps(sarif_output, indent=2, sort_keys=True), "results.sarif"
    )
