#
# Copyright (c) 2021-2024 Semgrep Inc.
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
from tests.fixtures import RunSemgrep


# Running semgrep with multiple configs should fail fast if any of them have errors
@pytest.mark.kinda_slow
def test_multi_config_fail(run_semgrep_in_tmp: RunSemgrep):
    run_semgrep_in_tmp(
        [
            "rules/multi_config_fail/error.yaml",
            "rules/multi_config_fail/no_error.yaml",
        ],
        target_name="basic/stupid.py",
        assert_exit_code=7,
    )
