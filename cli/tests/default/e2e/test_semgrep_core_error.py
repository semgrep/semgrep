#
# Copyright (c) 2024-2025 Semgrep Inc.
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
import json

import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# it should not fail the whole scan for unhandled exn (e.g., r2c_was_fatal)
# that could still be attached to a file in semgrep-core
@pytest.mark.kinda_slow
def test_exit_code_warning_error(run_semgrep_in_tmp: RunSemgrep):
    stdout, stderr = run_semgrep_in_tmp(
        config=f"rules/stuff_stmt.yaml",
        target_name=f"error_management/r2c_was_fatal.py",
        options=["--verbose"],
        output_format=OutputFormat.TEXT,
        strict=False,
        assert_exit_code=0,
    )


# it should fail the scan if using --strict though
@pytest.mark.kinda_slow
def test_exit_code_with_strict(run_semgrep_in_tmp: RunSemgrep):
    stdout, stderr = run_semgrep_in_tmp(
        config=f"rules/stuff_stmt.yaml",
        target_name=f"error_management/r2c_was_fatal.py",
        options=["--verbose"],
        output_format=OutputFormat.TEXT,
        strict=True,
        assert_exit_code=2,
    )


# it should not fail the whole scan for exn in a post processor (e.g., nosemgrep)
@pytest.mark.kinda_slow
def test_error_management_post_processor(run_semgrep_in_tmp: RunSemgrep):
    stdout, stderr = run_semgrep_in_tmp(
        config=f"rules/stuff_stmt.yaml",
        target_name=f"error_management/r2c_nosemgrep_was_fatal.py",
        options=["--verbose"],
        output_format=OutputFormat.TEXT,
        strict=False,
        assert_exit_code=0,
    )


# semgrep/semgrep#12055: a taint chain longer than the default limit of 50
# tainted variables per function loses the flow, so the scan must warn about
# it in the JSON errors instead of staying silent. The warning must not fail
# the scan on its own (strict=False, exit code 0).
@pytest.mark.kinda_slow
def test_taint_max_tainted_vars_warning(run_semgrep_in_tmp: RunSemgrep):
    stdout, stderr = run_semgrep_in_tmp(
        config="rules/taint_max_tainted_vars.yaml",
        target_name="TaintMaxTaintedVars.java",
        output_format=OutputFormat.JSON,
        strict=False,
        assert_exit_code=0,
    )
    parsed = json.loads(stdout)
    assert parsed["results"] == []
    warnings = [
        error
        for error in parsed["errors"]
        if error["error_type"] == "SemgrepWarning"
        and "tainted variables" in error["message"]
    ]
    assert len(warnings) == 1
    assert "50" in warnings[0]["message"]
