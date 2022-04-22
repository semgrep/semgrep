import pytest


@pytest.mark.slow
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
        (
            "rules/join_rules/inline/inline-rules.yaml",
            "join_rules/user-input-with-unescaped-extension",
        ),
    ],
)
def test_join_rules(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0],
        "results.json",
    )


@pytest.mark.slow
@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/join_rules/recursive/java-callgraph-example/vulnado-sqli.yaml",
            "join_rules/recursive/java-callgraph-example/vulnado",
        ),
        (
            "rules/join_rules/recursive/java-callgraph-example/vulnado-sqli.yaml",
            "join_rules/recursive/java-callgraph-example/vulnado-chain-broken",
        ),
        (
            "rules/join_rules/recursive/flask-deep-stored-xss-example/flask-stored-xss.yaml",
            "join_rules/recursive/flask-deep-stored-xss-example",
        ),
    ],
)
def test_recursive_join_rules(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0], "results.json"
    )
