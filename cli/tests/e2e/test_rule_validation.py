import json

import pytest
from tests.conftest import _clean_stdout
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/invalid-rules/invalid-metavariable-regex.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-pattern-child.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-missing-top-item.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-pattern.yaml", "basic/stupid.py"),
    ],
)
def test_validation_of_invalid_rules(
    run_semgrep_in_tmp: RunSemgrep, snapshot, rule, target
):
    stdout, _ = run_semgrep_in_tmp(rule, target_name=target, assert_exit_code={2, 7})

    semgrep_json_output = json.loads(_clean_stdout(stdout))

    snapshot.assert_match(
        json.dumps(semgrep_json_output, indent=2, sort_keys=True), "results.json"
    )
