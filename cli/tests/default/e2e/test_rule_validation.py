import pytest
from tests.conftest import mask_floats
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

invalid_rules = [
    "rules/invalid-rules/invalid-metavariable-regex.yaml",
    "rules/invalid-rules/invalid-pattern-child.yaml",
    "rules/invalid-rules/invalid-missing-top-item.yaml",
    "rules/invalid-rules/invalid-pattern.yaml",
    "rules/invalid-rules/invalid-pattern-operator.yaml",
    "rules/invalid-rules/invalid-paths-list.yaml",
    "rules/invalid-rules/additional-invalid-pattern-operator.yaml",
    "rules/invalid-rules/string-pattern.yaml",
    "rules/invalid-rules/string-pattern-under-patterns.yaml",
    "rules/invalid-rules/missing-hyphen.yaml",
    "rules/invalid-rules/missing-pattern.yaml",
]

parametrized_invalid_rules = [
    (rule, schema_validation)
    for rule in invalid_rules
    for schema_validation in [True, False]
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,schema_validation",
    parametrized_invalid_rules,
    ids=[
        f"{rule}-old" if schema_validation else rule
        for rule, schema_validation in parametrized_invalid_rules
    ],
)
@pytest.mark.osemfail
@skip_on_windows  # better backslash replacement logic
def test_validation_of_invalid_rules(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule, schema_validation: bool
):
    options = ["--validate"]
    if not schema_validation:
        options.append("--x-no-python-schema-validation")

    _, err = run_semgrep_in_tmp(
        rule,
        options=options,
        output_format=OutputFormat.TEXT,
        assert_exit_code={2, 4},
    )

    posix_snapshot.assert_match(
        err,
        "results.txt",
    )


parametrized_top_level_valid = [
    ("rules/extra_field.yaml", True),
    ("rules/extra_field.yaml", False),
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,schema_validation",
    parametrized_top_level_valid,
    ids=[
        f"{rule}-old" if schema_validation else rule
        for rule, schema_validation in parametrized_top_level_valid
    ],
)
@pytest.mark.osemfail
def test_extra_top_level_valid(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule, schema_validation: bool
):
    """
    An extra field in the rule does not cause it to fail validation
    """
    options = ["--validate"]
    if not schema_validation:
        options.append("--x-no-python-schema-validation")

    _, err = run_semgrep_in_tmp(
        rule,
        options=options,
        output_format=OutputFormat.TEXT,
        assert_exit_code={0},
    )

    posix_snapshot.assert_match(
        mask_floats(err),
        "results.txt",
    )


valid_rules = [
    "rules/regex/regex-capture-groups.yaml",
    "rules/regex/numeric-regex-capture-rule.yaml",
    "rules/patternless-sca-rule.yaml",
]

parametrized_valid_rules = [
    (rule, schema_validation)
    for rule in valid_rules
    for schema_validation in [True, False]
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,schema_validation",
    parametrized_valid_rules,
    ids=[
        f"{rule}-old" if schema_validation else rule
        for rule, schema_validation in parametrized_valid_rules
    ],
)
def test_validation_of_valid_rules(
    run_semgrep_in_tmp: RunSemgrep, rule, schema_validation: bool
):
    options = ["--validate"]
    if not schema_validation:
        options.append("--x-no-python-schema-validation")

    run_semgrep_in_tmp(
        rule,
        options=options,
        output_format=OutputFormat.TEXT,
        assert_exit_code=0,
    )
