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
"""Unit tests for ``_baseline_scan_failure_suppression_sets`` in ``run_scan``."""
from pathlib import Path

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepCoreError
from semgrep.run_scan import _baseline_scan_failure_suppression_sets


def _scan_failure_at(
    path: str, *, rule_id: str | None, error_type: out.ErrorType | None = None
) -> SemgrepCoreError:
    """Helper to create a SemgrepCoreError at the given path with the given rule ID and error type."""
    et = error_type if error_type is not None else out.ErrorType(out.Timeout())
    return SemgrepCoreError(
        code=2,
        level=out.ErrorSeverity(out.Error_()),
        spans=None,
        core=out.CoreError(
            error_type=et,
            severity=out.ErrorSeverity(out.Error_()),
            location=out.Location(
                path=out.Fpath(path),
                start=out.Position(line=1, col=1, offset=0),
                end=out.Position(line=1, col=1, offset=0),
            ),
            message="test",
            details=None,
            rule_id=None if rule_id is None else out.RuleId(rule_id),
        ),
    )


@pytest.mark.quick
def test_suppression_sets_empty_when_no_errors():
    fw, rr = _baseline_scan_failure_suppression_sets([])
    assert fw == set()
    assert rr == set()


@pytest.mark.quick
def test_suppression_sets_file_wide_vs_rule_scoped():
    errs = [
        _scan_failure_at("f1.py", rule_id="rule-a"),
        _scan_failure_at("f2.py", rule_id=None),
    ]
    fw, rr = _baseline_scan_failure_suppression_sets(errs)
    assert fw == {Path("f2.py")}
    assert rr == {(Path("f1.py"), "rule-a")}


@pytest.mark.quick
def test_suppression_sets_ignores_non_scan_failures():
    errs = [
        _scan_failure_at(
            "x.py",
            rule_id=None,
            error_type=out.ErrorType(out.LexicalError()),
        ),
    ]
    fw, rr = _baseline_scan_failure_suppression_sets(errs)
    assert fw == set()
    assert rr == set()
