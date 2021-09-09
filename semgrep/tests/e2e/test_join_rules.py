import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/join_rules/user-input-escaped-with-safe.yaml",
            "join_rules/user-input-escaped-with-safe",
        ),
        (
            "rules/join_rules/user-input-with-unescaped-extension.yaml",
            "join_rules/user-input-with-unescaped-extension",
        ),
        (
            "rules/join_rules/multiple-rules.yaml",
            "join_rules/user-input-with-unescaped-extension",
        ),
    ],
)
def test_join_rules(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0],
        "results.json",
    )
