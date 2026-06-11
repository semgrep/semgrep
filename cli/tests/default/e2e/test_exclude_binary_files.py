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
@pytest.mark.parametrize(
    "options",
    [
        # Default behavior: binary files (e.g. the PNG) are skipped.
        ["--x-ls"],
        # Opt out: binary files are kept in the target list.
        ["--x-ls", "--no-exclude-binary-files"],
    ],
    ids=["exclude-binary-files", "no-exclude-binary-files"],
)
def test_exclude_binary_files_file_list(
    run_semgrep_in_test_folder: RunSemgrep, posix_snapshot, options
):
    stdout, _stderr = run_semgrep_in_test_folder(
        "rules/eqeq.yaml",  # unused; --x-ls only lists targets
        options=options,
        target_name="binary_files",
        osemgrep_force_project_root=".",
        assert_exit_code=None,
    )
    posix_snapshot.assert_match(stdout, "files.list")
