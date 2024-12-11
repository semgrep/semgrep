import json
import shutil
from pathlib import Path

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# It should generate the right *fixed* fingerprint based on the rule and match.
# The fingerprint should not change across semgrep versions; it must remain
# stable.
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule_and_target",
    [
        # similar to test_output_sarif.py
        ("rules/eqeq.yaml", "basic/stupid.py"),
        ("rules/cwe_tag.yaml", "basic/stupid.py"),
    ],
)
@pytest.mark.kinda_slow
def test_value(run_semgrep_in_tmp: RunSemgrep, snapshot, rule_and_target):
    (rule, target) = rule_and_target
    results, _errors = run_semgrep_in_tmp(
        rule,
        target_name=target,
        output_format=OutputFormat.JSON,
        clean_fingerprint=False,
        is_logged_in_weak=True,
    )
    snapshot.assert_match(results, "results.json")


# TODO: this currently don't pass with osemgrep, mostly because
# of rule.py formula_string() not being fully ported to osemgrep
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule_and_target",
    [("rules/taint_trace.yaml", "taint/taint_trace.cpp")],
)
@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_value_osemfail(run_semgrep_in_tmp: RunSemgrep, snapshot, rule_and_target):
    (rule, target) = rule_and_target
    results, _errors = run_semgrep_in_tmp(
        rule,
        target_name=target,
        output_format=OutputFormat.JSON,
        clean_fingerprint=False,
        is_logged_in_weak=True,
    )
    snapshot.assert_match(results, "results.json")


@pytest.mark.kinda_slow
def test_duplicate_matches_indexing(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _errors = run_semgrep_in_tmp(
        "rules/match_based_id/duplicates.yaml",
        target_name="match_based_id/duplicates",
        output_format=OutputFormat.JSON,
        clean_fingerprint=False,
        # we now need to be logged in to access match_based_id fingerprints
        is_logged_in_weak=True,
    )
    snapshot.assert_match(results, "results.json")


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target_name,expect_change",
    [],
)
def test_id_change(
    run_semgrep_on_copied_files: RunSemgrep, tmp_path, rule, target_name, expect_change
):
    """
    Ensures that match-based IDs are resistant to various types of changes in
    code.

    These changes are enumerated in
       targets / match_based_id / (before|after) / <target_name>

    To edit these cases, edit these files directly. To add new cases, add a
    corresponding pair
    of files, and update the parameterization above.

    :param rule: The Semgrep rule that should trigger a finding
    :param target: The filename of the target pair
    :param expect_change: Whether or not to expect an ID change
    """

    # Since the match_based_id includes the target path, we must create a static
    # target path.
    # Note that b/c we use run_semgrep_on_copied_files, the current working
    # directory for Semgrep is tmp_path; I specify the target as an absolute
    # path so that things will bomb out if, for whatever reason, the working
    # directory changes.
    static_target = (
        tmp_path / "targets" / ("_match_based_id" + Path(target_name).suffix)
    )

    def run_on_target(subpath):
        source_target = tmp_path / "targets" / "match_based_id" / subpath / target_name
        shutil.copy(source_target, static_target)
        results, _ = run_semgrep_on_copied_files(
            rule,
            target_name=static_target,
            output_format=OutputFormat.JSON,
            clean_fingerprint=False,
            is_logged_in_weak=True,
        )
        return json.loads(results)["results"][0]["extra"]["fingerprint"]

    before_id = run_on_target("before")
    after_id = run_on_target("after")

    assert (after_id != before_id) == expect_change


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target_name,expect_change",
    [
        ("rules/match_based_id/formatting.yaml", "formatting.c", False),
        ("rules/match_based_id/formatting.yaml", "ellipse.c", False),
        ("rules/taint.yaml", "taint.py", False),
        # ("rules/match_based_id/","",True)
        ("rules/match_based_id/operator.yaml", "operator.c", True),
        ("rules/match_based_id/formatting.yaml", "meta-change.c", True),
        ("rules/match_based_id/join.yaml", "join.py", True),
    ],
)
@pytest.mark.osemfail
def test_id_change_osemfail(
    run_semgrep_on_copied_files: RunSemgrep, tmp_path, rule, target_name, expect_change
):
    test_id_change(
        run_semgrep_on_copied_files, tmp_path, rule, target_name, expect_change
    )
