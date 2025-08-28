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
import subprocess

import pytest
from tests.fixtures import RunSemgrep

pytestmark = pytest.mark.kinda_slow


def _git_commit() -> str:
    subprocess.run(
        ["git", "commit", "-m", "test commit"],
        check=True,
        capture_output=True,
    )
    return subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()


def _get_git_status() -> str:
    return subprocess.check_output(
        ["git", "status", "--porcelain"], encoding="utf-8"
    ).strip()


@pytest.mark.kinda_slow
def test_diff_scan_preserves_repo(git_tmp_path, run_semgrep: RunSemgrep):
    # Create baseline commit with a file that has a finding
    initial_file = git_tmp_path / "initial.py"
    initial_file.write_text("x = 42")
    subprocess.run(["git", "add", "."], check=True, capture_output=True)
    base_commit = _git_commit()

    # Make a second commit with the same file (so it exists in both baseline and current)
    initial_file.write_text("y = 42")
    subprocess.run(["git", "add", "initial.py"], check=True, capture_output=True)
    _git_commit()

    # Create the rule file (untracked)
    rule_file = git_tmp_path / "test_rule.yaml"
    rule_file.write_text(
        """
rules:
  - id: test-rule
    pattern: $X = 42
    message: Test finding
    languages: [python]
    severity: WARNING
"""
    )

    # Staged change
    staged_file = git_tmp_path / "staged.py"
    staged_file.write_text("z = 42\n")
    subprocess.run(["git", "add", "staged.py"], check=True, capture_output=True)

    # Uncommitted changes
    initial_file.write_text("z = 42")

    # Capture git status before running semgrep
    git_status_before = _get_git_status()
    assert "test_rule.yaml" in git_status_before
    assert "staged.py" in git_status_before
    assert "initial.py" in git_status_before

    # Diff scan
    run_semgrep(
        config=str(rule_file),
        options=["--baseline-commit", base_commit],
        assert_exit_code=0,
    )

    # Git status is preserved
    git_status_after = _get_git_status()
    assert git_status_before == git_status_after, (
        f"Git status changed after semgrep scan!\n"
        f"Before: {repr(git_status_before)}\n"
        f"After:  {repr(git_status_after)}"
    )
    assert "test_rule.yaml" in git_status_after
    assert "staged.py" in git_status_after
    assert "initial.py" in git_status_after
