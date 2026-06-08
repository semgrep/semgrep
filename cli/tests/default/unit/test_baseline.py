#
# Copyright (c) 2022-2024 Semgrep Inc.
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
import os
import subprocess
from pathlib import Path

import pytest

from semgrep.git import BaselineHandler


@pytest.mark.kinda_slow
def test_baseline_context(monkeypatch, tmp_path):
    """
    Unit test verifies baseline_context can checkout a commit and return to
    old state
    """
    monkeypatch.chdir(tmp_path)

    # Initialize State
    subprocess.check_call(["git", "init"])
    subprocess.check_call(["git", "config", "user.email", "baselinetest@semgrep.com"])
    subprocess.check_call(["git", "config", "user.name", "Baseline Test"])
    subprocess.check_call(["git", "checkout", "-B", "main"])

    # Create foo/a.py, foo/b.py, and foo/c.py
    foo = Path("foo")
    foo.mkdir()
    foo_a = foo / "a.py"
    foo_a.touch()
    foo_b = foo / "b.py"
    foo_b.write_text("z = 7777777")
    foo_c = foo / "c.py"
    foo_c.write_text("x = 11111111\n")

    # Add and commit foo/a.py, foo/b.py, and foo/c.py
    subprocess.check_call(["git", "add", "."])
    subprocess.check_call(["git", "commit", "-m", "first"])
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Create bar/a.py and modify foo/a.py and remove foo/b.py and rename foo/c.py
    bar = Path("bar")
    bar.mkdir()
    bar_a = bar / "a.py"
    bar_a.touch()
    foo_a.write_text("y = 55555555\n")
    foo_b.unlink()
    foo_d = foo / "d.py"
    foo_c.rename(foo_d)

    # Add and commit all changes
    subprocess.check_call(["git", "add", "."])
    # baseline_handler works on pending changes
    baseline_handler = BaselineHandler(base_commit)

    with baseline_handler.baseline_context():
        # foo/a.py should not have written text
        assert foo_a.read_text() == ""
        assert foo_b.exists()
        assert foo_c.exists()

        assert not bar_a.exists()
        assert not foo_d.exists()

    subprocess.check_call(["git", "commit", "-m", "second"])

    baseline_handler = BaselineHandler(base_commit)
    with baseline_handler.baseline_context():
        # foo/a.py should not have written text
        assert foo_a.read_text() == ""
        assert foo_b.exists()
        assert foo_c.exists()

        assert not bar_a.exists()
        assert not foo_d.exists()

    assert foo_a.read_text() == "y = 55555555\n"
    assert not foo_b.exists()
    assert not foo_c.exists()
    assert bar_a.exists()
    assert foo_d.read_text() == "x = 11111111\n"


@pytest.mark.kinda_slow
def test_baseline_context_with_relative_git_index_file_env(monkeypatch, tmp_path):
    """
    Regression test for the pre-commit hook ENOTDIR bug.

    `git commit` exports GIT_INDEX_FILE=.git/index (relative) into the
    pre-commit hook environment. When baseline_context() chdirs into a
    temp worktree, that inherited relative path re-resolves to
    <tmp>/.git/index. <tmp>/.git is a gitfile (regular file), so any
    subprocess that honors the inherited env and tries to lock the index
    fails with 'Unable to create <tmp>/.git/index.lock: Not a directory'.

    baseline_context must scrub GIT_INDEX_FILE (and the other git
    local-env-vars) before its git subprocesses.
    """
    monkeypatch.chdir(tmp_path)

    subprocess.check_call(["git", "init"])
    subprocess.check_call(["git", "config", "user.email", "baselinetest@semgrep.com"])
    subprocess.check_call(["git", "config", "user.name", "Baseline Test"])
    subprocess.check_call(["git", "checkout", "-B", "main"])

    Path("a.py").write_text("x = 1\n")
    subprocess.check_call(["git", "add", "a.py"])
    subprocess.check_call(["git", "commit", "-m", "first"])
    base_commit = subprocess.check_output(
        ["git", "rev-parse", "HEAD"], encoding="utf-8"
    ).strip()

    # Stage an unrelated change so the repo is "dirty" and baseline_context
    # takes the git-worktree branch (the buggy code path).
    Path("a.py").write_text("x = 2\n")
    subprocess.check_call(["git", "add", "a.py"])

    # Simulate exactly the env that `git commit -m` exports to pre-commit
    # hooks on a typical git installation: GIT_INDEX_FILE as a relative
    # path. Without the env scrub in baseline_context, this would explode
    # with ENOTDIR when git checkout tries to lock <tmp>/.git/index.lock.
    monkeypatch.setenv("GIT_INDEX_FILE", ".git/index")

    baseline_handler = BaselineHandler(base_commit)
    with baseline_handler.baseline_context():
        # If we got here, the scrub worked — the worktree was created and
        # checked out without ENOTDIR.
        assert Path("a.py").read_text() == "x = 1\n"

    # Confirm the env var is restored after the context exits.
    assert os.environ["GIT_INDEX_FILE"] == ".git/index"
