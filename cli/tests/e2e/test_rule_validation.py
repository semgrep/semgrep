import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,target",
    [
        ("rules/invalid-rules/invalid-metavariable-regex.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-pattern-child.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-missing-top-item.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/invalid-pattern.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/string-pattern.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/string-pattern-under-patterns.yaml", "basic/stupid.py"),
        ("rules/invalid-rules/missing-hyphen.yaml", "basic/stupid.py"),
    ],
)
def test_validation_of_invalid_rules(
    run_semgrep_in_tmp: RunSemgrep, snapshot, rule, target
):
    _, err = run_semgrep_in_tmp(
        rule,
        options=["--validate"],
        output_format=OutputFormat.TEXT,
        assert_exit_code={2, 4},
    )

    snapshot.assert_match(
        err,
        "results.txt",
    )


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule",
    ["rules/regex-capture-groups.yaml", "rules/numeric-regex-capture-rule.yaml"],
)
def test_validation_of_valid_rules(run_semgrep_in_tmp: RunSemgrep, rule):
    run_semgrep_in_tmp(
        rule,
        options=["--validate"],
        output_format=OutputFormat.TEXT,
        assert_exit_code=0,
    )
