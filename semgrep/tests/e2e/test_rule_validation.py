import json
from subprocess import CalledProcessError

import pytest


@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/invalid-rules/invalid-metavariable-regex.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-pattern-child.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-missing-top-item.yaml", "basic/stupid.py"),
    ],
)
def test_validation_of_invalid_rules(run_semgrep_in_tmp, snapshot, rule, target):
    with pytest.raises(CalledProcessError) as exc_info:
        run_semgrep_in_tmp(rule, target_name=target)

    semgrep_json_output = json.loads(exc_info.value.stdout)

    snapshot.assert_match(
        json.dumps(semgrep_json_output, indent=2, sort_keys=True), "results.json"
    )
