#
# Copyright (c) 2025 Semgrep Inc.
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
"""Unit tests for ``_raise_skipped_rule_validation_errors`` in ``run_scan``."""
import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import MISSING_CONFIG_EXIT_CODE
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.rule_lang import RuleValidationMode
from semgrep.run_scan import _raise_skipped_rule_validation_errors


def _core_error(error_type: out.ErrorType) -> SemgrepCoreError:
    return SemgrepCoreError(
        code=2,
        level=out.ErrorSeverity(out.Error_()),
        spans=None,
        core=out.CoreError(
            error_type=error_type,
            severity=out.ErrorSeverity(out.Error_()),
            location=out.Location(
                path=out.Fpath("rules.yaml"),
                start=out.Position(line=1, col=1, offset=0),
                end=out.Position(line=1, col=1, offset=0),
            ),
            message="bad rule",
            details=None,
            rule_id=out.RuleId("eqeq"),
        ),
    )


@pytest.mark.quick
def test_raises_invalid_rule_schema_under_none():
    err = _core_error(out.ErrorType(out.RuleParseError()))

    with pytest.raises(SemgrepError) as exc_info:
        _raise_skipped_rule_validation_errors([err], RuleValidationMode.NONE)

    assert exc_info.value.code == MISSING_CONFIG_EXIT_CODE
    assert str(exc_info.value).startswith("Invalid rule schema")


@pytest.mark.quick
def test_no_raise_under_full_even_with_rule_parse_error():
    err = _core_error(out.ErrorType(out.RuleParseError()))

    _raise_skipped_rule_validation_errors([err], RuleValidationMode.FULL)


@pytest.mark.quick
def test_no_raise_under_none_when_no_rule_parse_errors():
    err = _core_error(out.ErrorType(out.LexicalError()))

    _raise_skipped_rule_validation_errors([err], RuleValidationMode.NONE)
