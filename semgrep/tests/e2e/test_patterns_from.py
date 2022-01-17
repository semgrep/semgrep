import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/patterns-from/taint-test.yaml",
            "patterns-from/taint-test.py",
        ),
        (
            "rules/patterns-from/pattern-addr-test.yaml",
            "patterns-from/pattern-addr-test.js",
        ),
        (
            "rules/patterns-from/message-override-test.yaml",
            "patterns-from/message-override-test.kt",
        ),
    ],
)
def test_patterns_from(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0],
        "results.json",
    )
