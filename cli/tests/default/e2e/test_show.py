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
#
# Tests for the 'semgrep show' subcommands
#
import os
import subprocess
from pathlib import Path

import pytest
from tests.semgrep_runner import mk_semgrep_base_command


@pytest.mark.kinda_slow
def test_show_project_root(snapshot):
    stdout = subprocess.check_output(
        mk_semgrep_base_command("show", ["project-root", "."]),
        encoding="utf-8",
    )
    project_root = Path(stdout.rstrip())
    print(f"project root: {project_root}")
    # Check that the detected project root is a Git project root
    git_folder_or_file = project_root / ".git"
    assert os.path.exists(git_folder_or_file)
