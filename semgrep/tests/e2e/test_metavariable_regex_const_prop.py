import pytest


@pytest.mark.kinda_slow
def test_metavariable_regex_const_prop(run_semgrep_in_tmp, snapshot):
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-regex-const-prop.yaml",
            target_name="metavariable_propagation/metavariable-regex-const-prop.dockerfile",
        )[0],
        "results.json",
    )
