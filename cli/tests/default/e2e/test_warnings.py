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
# Test various warnings and error messages
import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_semgrepignore_v2_warning(
    run_semgrep_on_copied_files: RunSemgrep, posix_snapshot
):
    """Check that '--semgrepignore-v2' prints a deprecation warning"""
    posix_snapshot.assert_match(
        run_semgrep_on_copied_files(
            config="rules/eqeq.yaml",
            target_name="basic",
            output_format=OutputFormat.TEXT,
            options=["--semgrepignore-v2"],
        ).stderr,
        "results.txt",
    )
