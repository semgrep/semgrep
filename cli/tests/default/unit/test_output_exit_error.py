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
from typing import List
from typing import Optional

import pytest
from pytest_mock import MockerFixture

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import OutputFormat
from semgrep.error import SemgrepError
from semgrep.output import OutputHandler
from semgrep.output import OutputSettings
from semgrep.types import TargetInfoAccumulator

ERROR = out.ErrorSeverity(out.Error_())
WARN = out.ErrorSeverity(out.Warning_())


def _errors(levels: List[out.ErrorSeverity]) -> List[SemgrepError]:
    # distinct codes, so the raised error identifies its position in the list
    return [
        SemgrepError(f"error {i}", code=10 + i, level=level)
        for i, level in enumerate(levels)
    ]


@pytest.mark.quick
@pytest.mark.no_semgrep_cli
@pytest.mark.parametrize(
    ("levels", "strict", "raised_index"),
    [
        ([ERROR, WARN], False, 0),
        ([WARN, ERROR], False, 1),
        ([WARN, ERROR, WARN], True, 1),
        ([WARN, WARN], False, None),
        ([WARN, WARN], True, 1),
    ],
)
def test_exit_code_comes_from_the_highest_level_error(
    levels: List[out.ErrorSeverity],
    strict: bool,
    raised_index: Optional[int],
    mocker: MockerFixture,
) -> None:
    # formatting is an RPC into semgrep-core; the exit code is decided before it
    mocker.patch("semgrep.rpc_call.format", return_value="")
    handler = OutputHandler(
        OutputSettings(output_format=OutputFormat.JSON, strict=strict)
    )
    errors = _errors(levels)
    handler.handle_semgrep_errors(errors)

    def run() -> None:
        handler.output({}, all_targets_acc=TargetInfoAccumulator(), filtered_rules=[])

    if raised_index is None:
        run()
    else:
        with pytest.raises(SemgrepError) as raised:
            run()
        assert raised.value is errors[raised_index]
