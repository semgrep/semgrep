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
import subprocess
from types import SimpleNamespace

import pytest

import semgrep.scan_report as scan_report
from semgrep.state import DesignTreatment
from semgrep.target_manager import TargetManager


def _print_scan_plan_header(tmp_path, monkeypatch, *, root, cli_ux):
    """
    Call _print_scan_plan_header for a single scanning root and return
    the exact line it passes to console.print.

    Target discovery is stubbed out (it needs the semgrep-core RPC
    binary); the header only uses the number of targets. console.print
    is recorded instead of captured, because the global console is
    silenced (quiet=True) whenever the library-default Terminal is
    constructed outside a real CLI invocation.
    """
    printed: list[str] = []
    monkeypatch.setattr(
        scan_report.console,
        "print",
        lambda *args, **kwargs: printed.append(" ".join(str(a) for a in args)),
    )
    monkeypatch.setattr(
        TargetManager,
        "get_all_files",
        lambda self, product=None: [object()] * 3,
    )

    monkeypatch.chdir(tmp_path)
    target_manager = TargetManager(
        scanning_root_strings=frozenset([root]),
        respect_git_ignore=True,
    )
    empty_plan = SimpleNamespace(rules=[])
    scan_report._print_scan_plan_header(
        target_manager, empty_plan, empty_plan, None, cli_ux
    )
    assert len(printed) == 1
    return printed[0]


@pytest.mark.quick
def test_print_scan_plan_header_no_suffix_outside_git_repo(tmp_path, monkeypatch):
    """
    Outside a git repo, the scan-plan header must not claim the scan was
    limited to git-tracked files (issue #9815), in either CLI UX.
    """
    root = tmp_path / "proj"
    root.mkdir()
    (root / "a.py").touch()

    assert (
        _print_scan_plan_header(
            tmp_path, monkeypatch, root=root, cli_ux=DesignTreatment.LEGACY
        )
        == "Scanning 3 files:"
    )
    assert (
        _print_scan_plan_header(
            tmp_path, monkeypatch, root=root, cli_ux=DesignTreatment.SIMPLE
        )
        == "Scanning 3 files with:"
    )


@pytest.mark.quick
def test_print_scan_plan_header_suffix_inside_git_repo(tmp_path, monkeypatch):
    """
    Inside a git repo, the scan-plan header still advertises the
    git-tracked limitation.
    """
    root = tmp_path / "proj"
    root.mkdir()
    (root / "a.py").touch()

    subprocess.run(["git", "init"], cwd=root)
    subprocess.run(["git", "add", root / "a.py"], cwd=root)

    assert (
        _print_scan_plan_header(
            tmp_path, monkeypatch, root=root, cli_ux=DesignTreatment.LEGACY
        )
        == "Scanning 3 files tracked by git:"
    )
    assert (
        _print_scan_plan_header(
            tmp_path, monkeypatch, root=root, cli_ux=DesignTreatment.SIMPLE
        )
        == "Scanning 3 files (only git-tracked) with:"
    )
