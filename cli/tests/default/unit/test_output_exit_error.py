#
# Copyright (c) 2026 Semgrep Inc.
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
"""
Which structured error decides the exit code (gh-11960): reading only the
last one let a warning reported after an error-level failure turn the exit
code into 0.
"""
from typing import Callable
from typing import Dict
from typing import List
from typing import Optional

import pytest
from pytest_mock import MockerFixture

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import OutputFormat
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.types import TargetInfoAccumulator

ERROR = out.ErrorSeverity(out.Error_())
WARN = out.ErrorSeverity(out.Warning_())


def _error(i: int, level: out.ErrorSeverity) -> SemgrepError:
    return SemgrepError(f"error {i}", code=10 + i, level=level)


def _interfile_error(i: int) -> SemgrepError:
    # error-level, yet is_special_interfile_analysis_error keeps it from
    # blocking outside --strict
    return SemgrepCoreError(
        code=10 + i,
        level=ERROR,
        spans=None,
        core=out.CoreError(
            error_type=out.ErrorType(out.TimeoutDuringInterfile()),
            severity=ERROR,
            message=f"error {i}",
        ),
    )


_BUILDERS: Dict[str, Callable[[int], SemgrepError]] = {
    "error": lambda i: _error(i, ERROR),
    "warn": lambda i: _error(i, WARN),
    "interfile": _interfile_error,
}


def _errors(kinds: List[str]) -> List[SemgrepError]:
    # distinct codes, so the raised error identifies its position in the list
    return [_BUILDERS[kind](i) for i, kind in enumerate(kinds)]


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
@pytest.mark.parametrize(
    ("kinds", "strict", "raised_index"),
    [
        (["error", "warn"], False, 0),
        (["warn", "error"], False, 1),
        (["warn", "error", "warn"], True, 1),
        (["warn", "warn"], False, None),
        (["warn", "warn"], True, 1),
        # blocking outranks level: an interfile error is error-level but does
        # not block outside --strict
        (["error", "interfile"], False, 0),
        (["interfile", "error"], False, 1),
        (["interfile"], False, None),
        (["interfile"], True, 0),
        # equal on both: the last reported
        (["error", "error"], False, 1),
    ],
)
def test_exit_code_comes_from_the_blocking_highest_level_error(
    kinds: List[str],
    strict: bool,
    raised_index: Optional[int],
    mocker: MockerFixture,
) -> None:
    # formatting is an RPC into semgrep-core; the exit code is decided before it
    mocker.patch("semgrep.rpc_call.format", return_value="")
    handler = OutputHandler(
        OutputSettings(output_format=OutputFormat.JSON, strict=strict)
    )
    errors = _errors(kinds)
    handler.handle_semgrep_errors(errors)

    def run() -> None:
        handler.output({}, all_targets_acc=TargetInfoAccumulator(), filtered_rules=[])

    if raised_index is None:
        run()
    else:
        with pytest.raises(SemgrepError) as raised:
            run()
        assert raised.value is errors[raised_index]
