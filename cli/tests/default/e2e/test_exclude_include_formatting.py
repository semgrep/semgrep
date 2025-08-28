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
# These tests used to be in test_exclude_include but they're mostly about
# formatting results when files are skipped.
import pytest
from tests.conftest import skip_on_windows
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


# Test output formatting in verbose mode.
#
# Exclude all the files with '--exclude' patterns resulting in 0 files being
# scanned.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
@skip_on_windows  # better backslash replacement
def test_exclude_include_verbose_sorted_1(
    run_semgrep_on_copied_files: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
        run_semgrep_on_copied_files(
            "rules/eqeq.yaml",
            options=["--exclude", "excluded.*", "--exclude", "included.*", "--verbose"],
            output_format=OutputFormat.TEXT,
            target_name="exclude_include",
            assert_exit_code=None,
        ).stderr,
        "results.err",
    )


# Another test for output formatting in verbose mode.
#
# Exclude all the files with '--exclude' patterns resulting in 0 files being
# scanned.
@pytest.mark.kinda_slow
@pytest.mark.osemfail
@skip_on_windows  # better backslash replacement
def test_exclude_include_verbose_sorted_2(
    run_semgrep_on_copied_files: RunSemgrep, posix_snapshot
):
    posix_snapshot.assert_match(
        run_semgrep_on_copied_files(
            "rules/nosem.yaml",
            # OCaml runtime expands '*.*' arguments
            options=["--exclude=*.*", "--verbose"],
            output_format=OutputFormat.TEXT,
            target_name="basic",
            assert_exit_code=None,
        ).stderr,
        "results.err",
    )
