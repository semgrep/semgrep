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
