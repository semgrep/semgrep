import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_base(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison-base.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_strip(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison-strip.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_comparison_rule_bad_content(
    run_semgrep_in_tmp: RunSemgrep, snapshot
):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-comparison/metavariable-comparison-bad-content.yaml"
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test_metavariable_propagation_comparison(run_semgrep_in_tmp: RunSemgrep, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable_propagation/metavariable-comparison-propagation.yaml",
            target_name="metavariable_propagation/metavariable-comparison-propagation.py",
        ).stdout,
        "results.json",
    )
