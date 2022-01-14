import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        (
            "rules/dependency_aware/awscli_vuln.yaml",
            "dependency_aware/awscli_vuln.py",
        ),
        (
            "rules/dependency_aware/lodash-4.17.19.yaml",
            "dependency_aware/useslodash.js",
        ),
        # (
        #    "rules/dependency_aware/lodash-4.17.19.yaml",
        #    "dependency_aware_safe/useslodash.js",
        # ),
    ],
)
def test_dependency_aware_rules(run_semgrep_in_tmp, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target)[0],
        "results.json",
    )
