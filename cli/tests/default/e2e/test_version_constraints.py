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
@skip_on_windows  # better masking
def test_version_constraints(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/version-constraints.yaml", target_name="version-constraints/x.py"
        ).stdout,
        "results.json",
    )
