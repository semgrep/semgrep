#
# Copyright (c) 2023-2024 Semgrep Inc.
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

from semgrep.constants import OutputFormat

# was in e2e/test_output.py before


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_cli_test_secret_rule(run_semgrep_in_tmp: RunSemgrep, snapshot):
    results, _ = run_semgrep_in_tmp(
        "rules/secrets.yaml",
        target_name="basic.py",
        output_format=OutputFormat.TEXT,
        force_color=True,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )
