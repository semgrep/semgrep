#
# Copyright (c) 2021-2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import pytest
from tests.conftest import mask_floats
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

VALIDATION_MODES = ["full", "core-only", "none"]


def _validation_options(mode: str) -> list:
    # "full" is the default semgrep behavior, so don't bother passing the flag
    # for it; this also keeps the bare `--validate` case under test.
    if mode == "full":
        return ["--validate"]
    return ["--validate", f"--x-rule-validation={mode}"]


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
    (rule, mode) for rule in invalid_rules for mode in VALIDATION_MODES
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,validation_mode",
    parametrized_invalid_rules,
    ids=[f"{rule}-{mode}" for rule, mode in parametrized_invalid_rules],
)
@pytest.mark.osemfail
@skip_on_windows  # better backslash replacement logic
def test_validation_of_invalid_rules(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule, validation_mode: str
):
    _, err = run_semgrep_in_tmp(
        rule,
        options=_validation_options(validation_mode),
        output_format=OutputFormat.TEXT,
        assert_exit_code={2, 4},
    )

    posix_snapshot.assert_match(
        err,
        "results.txt",
    )


parametrized_top_level_valid = [
    ("rules/extra_field.yaml", mode) for mode in VALIDATION_MODES
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,validation_mode",
    parametrized_top_level_valid,
    ids=[f"{rule}-{mode}" for rule, mode in parametrized_top_level_valid],
)
@pytest.mark.osemfail
def test_extra_top_level_valid(
    run_semgrep_in_tmp: RunSemgrep, posix_snapshot, rule, validation_mode: str
):
    """
    An extra field in the rule does not cause it to fail validation
    """
    _, err = run_semgrep_in_tmp(
        rule,
        options=_validation_options(validation_mode),
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
    (rule, mode) for rule in valid_rules for mode in VALIDATION_MODES
]


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "rule,validation_mode",
    parametrized_valid_rules,
    ids=[f"{rule}-{mode}" for rule, mode in parametrized_valid_rules],
)
def test_validation_of_valid_rules(
    run_semgrep_in_tmp: RunSemgrep, rule, validation_mode: str
):
    run_semgrep_in_tmp(
        rule,
        options=_validation_options(validation_mode),
        output_format=OutputFormat.TEXT,
        assert_exit_code=0,
    )
