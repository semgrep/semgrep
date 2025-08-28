#
# Copyright (c) 2021-2025 Semgrep Inc.
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


def generic_test_max_target_bytes(
    run_semgrep_in_tmp: RunSemgrep,
    posix_snapshot,
    max_bytes,
    check_results=True,
    check_output=True,
):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic",
        options=["--max-target-bytes", max_bytes],
        assert_exit_code=None,
    )
    if check_results:
        posix_snapshot.assert_match(stdout, "results.json")
    if check_output:
        posix_snapshot.assert_match(stderr, "error.txt")


@pytest.mark.kinda_slow
@pytest.mark.parametrize("max_bytes", ["1MB", "1.3R", "100B", "1B"])
@pytest.mark.osemfail
def test_max_target_bytes_results(
    run_semgrep_on_copied_files: RunSemgrep, posix_snapshot, max_bytes
):
    generic_test_max_target_bytes(
        run_semgrep_on_copied_files,
        posix_snapshot,
        max_bytes,
        check_results=True,
        check_output=False,
    )


@pytest.mark.kinda_slow
@pytest.mark.parametrize("max_bytes", ["1MB", "1.3R"])
@pytest.mark.osemfail
def test_max_target_bytes_output(
    run_semgrep_in_test_folder: RunSemgrep, posix_snapshot, max_bytes
):
    generic_test_max_target_bytes(
        run_semgrep_in_test_folder,
        posix_snapshot,
        max_bytes,
        check_results=False,
        check_output=True,
    )


# The message we print is something like this:
#
#    Scanning 123 files with 456 Code rules
#
# pysemgrep prints an incorrect number of files because filtering based on
# file size and other ignore mechanisms is mixed with rule-specific filtering.
# It's hard to fix and osemgrep does this correctly, so we mark the test
# as failing with pysemgrep.
#
@pytest.mark.kinda_slow
@pytest.mark.parametrize("max_bytes", ["100B", "1B"])
@pytest.mark.pysemfail
def test_max_target_bytes_output_pysemfail(
    run_semgrep_on_copied_files: RunSemgrep, posix_snapshot, max_bytes
):
    test_max_target_bytes_output(run_semgrep_on_copied_files, posix_snapshot, max_bytes)
