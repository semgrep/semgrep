# Test rule ID syntax
import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/rule_id/@", "rule_id/hello.txt"),
        ("rules/rule_id/;", "rule_id/hello.txt"),
        ("rules/rule_id/@npm-style", "rule_id/hello.txt"),
    ],
)
def test_rule_id_paths(run_semgrep_in_tmp: RunSemgrep, snapshot, rule, target):
    snapshot.assert_match(
        run_semgrep_in_tmp(rule, target_name=target).stdout,
        "results.json",
    )
