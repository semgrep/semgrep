#
# Copyright (c) 2020-2025 Semgrep Inc.
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


@pytest.mark.kinda_slow
def test_equivalence(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp("rules/inside.yaml", target_name="basic").stdout,
        "results.json",
    )
