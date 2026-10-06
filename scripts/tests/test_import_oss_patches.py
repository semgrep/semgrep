#
# Copyright (c) 2026 Semgrep Inc.
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
"""Exercise commit imports against isolated Git repositories."""
import os
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[1] / "import-oss-patches.sh"


def git(repo, *args):
    return subprocess.check_output(["git", "-C", str(repo), *args])


def commit(repo, message):
    git(repo, "add", "-A")
    git(
        repo,
        "commit",
        "--allow-empty",
        "-qm",
        message,
        "--date=2020-01-02T03:04:05+02:00",
    )
    return git(repo, "rev-parse", "HEAD").decode().strip()


def run_import(repo, directory, *commits):
    return subprocess.run(
        ["bash", str(SCRIPT), directory, *commits],
        cwd=repo,
        capture_output=True,
        text=True,
    )


@pytest.fixture
def repos(tmp_path, monkeypatch):
    """Start clean repositories with distinct author and committer identities."""
    monkeypatch.setenv("GIT_CONFIG_GLOBAL", os.devnull)
    monkeypatch.setenv("GIT_CONFIG_NOSYSTEM", "1")
    monkeypatch.delenv("GIT_LFS_SKIP_SMUDGE", raising=False)
    source, target = tmp_path / "source", tmp_path / "target"
    for repo in (source, target):
        repo.mkdir()
        git(repo, "init", "-q")
        git(repo, "config", "user.name", repo.name)
        git(repo, "config", "user.email", f"{repo.name}@example.com")
        (repo / "parser.c").write_text("original\n")
        commit(repo, "base")
    return source, target


@pytest.mark.parametrize("directory", [".", "vendor"])
def test_import_lfs_commits(repos, directory):
    """Preserve commits and source-tracked paths using destination LFS rules."""
    source, target = repos
    base = git(source, "rev-parse", "HEAD").decode().strip()
    (source / "parser.c").write_text("updated\n")
    (source / "ignored file [1].c").write_text("tracked upstream\n")
    commit(source, "[PATCH] Keep this subject\n\nKeep this body.")
    git(source, "mv", "parser.c", "renamed.c")
    (source / "renamed.c").write_text("updated\nextra\n")
    commit(source, "Rename and edit")
    commit(source, "Empty source commit")
    commits = git(source, "rev-list", "--reverse", f"{base}..HEAD").decode().split()

    git(target, "lfs", "install", "--local")
    git(target, "fetch", "--no-tags", str(source), "HEAD")
    files = target / directory
    files.mkdir(exist_ok=True)
    prefix = "" if directory == "." else f"{directory}/"
    if prefix:
        git(target, "mv", "parser.c", f"{prefix}parser.c")
    (target / ".gitattributes").write_text(
        f"{prefix}*.c filter=lfs diff=lfs merge=lfs -text\n"
    )
    (target / ".gitignore").write_text(f"{prefix}ignored*.c\n{prefix}local-cache.c\n")
    git(target, "add", "--renormalize", "--", f"{prefix}parser.c")
    before = commit(target, "destination base")
    assert git(target, "show", f"{before}:{prefix}parser.c").startswith(
        b"version https://git-lfs.github.com/spec/v1\n"
    )
    (files / "local-cache.c").write_text("unrelated ignored contents\n")

    result = run_import(target, directory, *commits)
    assert result.returncode == 0, result.stdout + result.stderr
    metadata = "--format=%an%n%ae%n%aI%n%B"
    assert git(target, "log", "--reverse", metadata, f"{before}..HEAD") == git(
        source, "log", "--reverse", metadata, f"{base}..HEAD"
    )
    for name, content in {
        "renamed.c": "updated\nextra\n",
        "ignored file [1].c": "tracked upstream\n",
    }.items():
        assert (files / name).read_text() == content
        assert git(target, "show", f"HEAD:{prefix}{name}").startswith(
            b"version https://git-lfs.github.com/spec/v1\n"
        )
    assert not (files / "parser.c").exists()
    assert (files / "local-cache.c").read_text() == "unrelated ignored contents\n"
    assert git(target, "ls-files", "--", f"{prefix}local-cache.c") == b""
    assert git(target, "status", "--porcelain") == b""


def test_reject_conflict(repos):
    source, target = repos
    (source / "parser.c").write_text("updated\n")
    source_commit = commit(source, "source change")
    git(target, "fetch", "--no-tags", str(source), "HEAD")
    (target / "parser.c").write_text("conflicting\n")
    before = commit(target, "destination change")
    result = run_import(target, ".", source_commit)
    assert result.returncode != 0
    assert "patch does not apply" in result.stderr
    assert git(target, "rev-parse", "HEAD").decode().strip() == before
    assert (target / "parser.c").read_text() == "conflicting\n"
    assert git(target, "status", "--porcelain") == b""


def test_reject_dirty_checkout(repos):
    _, target = repos
    (target / "local.txt").write_text("uncommitted work\n")
    result = run_import(target, ".")
    assert result.returncode != 0
    assert "requires a clean checkout" in result.stderr
    assert (target / "local.txt").read_text() == "uncommitted work\n"


@pytest.mark.parametrize("total", [2, 3])
def test_workflow_rejects_incomplete_commit_list(total):
    workflow = (
        SCRIPT.parents[1] / ".github/workflows/sync-with-PRO.jsonnet"
    ).read_text()
    commands = (
        "PR_API="
        + workflow.split("        PR_API=", 1)[1].split(
            "        # Author's GitHub username", 1
        )[0]
    )
    result = subprocess.run(
        [
            "bash",
            "-e",
            "-c",
            'gh() { if [[ "$*" == *"/commits"* ]]; then printf "one\\ntwo\\n"; '
            f"else echo {total}; fi; }}\nPR_NUMBER=1\n" + commands,
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode == (0 if total == 2 else 1), result.stderr
    if total == 3:
        assert "incomplete PR commit list" in result.stderr


@pytest.mark.parametrize("change", ["add", "update", "delete"])
def test_reject_submodule_changes_before_importing(repos, change):
    """Reject the whole batch before applying its preceding ordinary commit."""
    source, target = repos
    base = git(source, "rev-parse", "HEAD").decode().strip()
    if change != "add":
        git(source, "update-index", "--add", "--cacheinfo", f"160000,{base},dependency")
        git(source, "commit", "-qm", "Submodule base")
    (source / "parser.c").write_text("updated\n")
    git(source, "add", "parser.c")
    git(source, "commit", "-qm", "Ordinary change")
    ordinary = git(source, "rev-parse", "HEAD").decode().strip()
    if change == "delete":
        git(source, "update-index", "--force-remove", "dependency")
    else:
        git(
            source,
            "update-index",
            "--add",
            "--cacheinfo",
            f"160000,{ordinary},dependency",
        )
    git(source, "commit", "-qm", "Submodule change")
    submodule = git(source, "rev-parse", "HEAD").decode().strip()
    git(target, "fetch", "--no-tags", str(source), "HEAD")
    before = git(target, "rev-parse", "HEAD")
    result = run_import(target, ".", ordinary, submodule)
    assert result.returncode != 0
    assert "submodule changes are not supported" in result.stderr
    assert submodule in result.stderr
    assert git(target, "rev-parse", "HEAD") == before
    assert (target / "parser.c").read_text() == "original\n"
    assert git(target, "status", "--porcelain") == b""
