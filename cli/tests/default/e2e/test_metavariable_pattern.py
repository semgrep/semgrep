#
# Copyright (c) 2023-2025 Semgrep Inc.
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
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep


@pytest.mark.kinda_slow
@skip_on_windows  # better backslash replacement logic
def test1(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    # https://github.com/returntocorp/semgrep/issues/7271
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-pattern/test1.json",
            target_name="metavariable-pattern/test1.yml",
            assert_exit_code=2,
        ).stdout,
        "results.json",
    )


@pytest.mark.kinda_slow
def test2(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    # https://linear.app/r2c/issue/PA-2696
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/metavariable-pattern/test2.yaml",
            target_name="metavariable-pattern/test2.php",
        ).stdout,
        "results.json",
    )
