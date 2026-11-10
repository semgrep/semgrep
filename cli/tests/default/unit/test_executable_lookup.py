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
# Testing src/semgrep/executable_lookup.py. Windows behavior is simulated so
# these run on every platform.
import functools
import importlib.resources
import os
import sys

import pytest

from semgrep import executable_lookup
from semgrep import semgrep_core
from semgrep.error import SemgrepError
from semgrep.git import git_check_output
from semgrep.util import sub_check_output

GIT = r"C:\Git\cmd\git.exe"


def simulate_windows(monkeypatch, path, files):
    """Pretend to be on Windows with PATH [path], where only [files] exist."""
    monkeypatch.setattr(executable_lookup, "IS_WINDOWS", True)
    monkeypatch.setenv("PATH", path)
    monkeypatch.setattr(
        executable_lookup,
        "search_windows_path",
        functools.partial(
            executable_lookup.search_windows_path, is_file=files.__contains__
        ),
    )


@pytest.mark.quick
@pytest.mark.parametrize(
    "name,path,files,expected",
    [
        # Implicit-CWD, empty, relative, drive-relative and root-relative
        # entries are skipped even when they contain a match.
        (
            "git",
            r".;;bin;C:Tools;\Root;C:\Git\cmd",
            {r".\git.exe", r"bin\git.exe", r"C:Tools\git.exe", r"\Root\git.exe", GIT},
            GIT,
        ),
        ("git", None, set(), None),
        # Quoted entries may contain ';'.
        (
            "git",
            r'"C:\Program Files;x\Git";C:\Git\cmd',
            {r"C:\Program Files;x\Git\git.exe", GIT},
            r"C:\Program Files;x\Git\git.exe",
        ),
        (
            "git",
            r"\\server\share\bin",
            {r"\\server\share\bin\git.exe"},
            r"\\server\share\bin\git.exe",
        ),
        (
            "git",
            r"\\server\share\bin;//other/share/",
            {"//other/share/git.exe"},
            "//other/share/git.exe",
        ),
        # Like CreateProcess, ".exe" is appended only without an extension.
        ("python3.11", r"C:\Py", {r"C:\Py\python3.11"}, r"C:\Py\python3.11"),
        ("git.EXE", r"C:\Git\cmd", {r"C:\Git\cmd\git.EXE"}, r"C:\Git\cmd\git.EXE"),
    ],
)
def test_search_windows_path(name, path, files, expected) -> None:
    assert (
        executable_lookup.search_windows_path(name, path, files.__contains__)
        == expected
    )


@pytest.mark.quick
def test_resolve_executable_only_overrides_windows_bare_names(monkeypatch) -> None:
    monkeypatch.setattr(executable_lookup, "IS_WINDOWS", False)
    monkeypatch.setenv("PATH", "relative")
    assert executable_lookup.resolve_executable(["git", "status"]) is None

    simulate_windows(monkeypatch, r"C:\Git\cmd", {GIT})
    for explicit in [r".\git", "C:git", "bin/git", GIT]:
        assert executable_lookup.resolve_executable([explicit]) is None


@pytest.mark.quick
def test_git_check_output_runs_git_found_in_path(monkeypatch, mocker) -> None:
    # PATHEXT is ignored: CreateProcess would not run git.cmd for "git".
    monkeypatch.setenv("PATHEXT", ".CMD;.EXE")
    simulate_windows(
        monkeypatch,
        r".;C:\Scripts;C:\Git\cmd",
        {r".\git.exe", r"C:\Scripts\git.cmd", GIT},
    )
    check_output = mocker.patch("subprocess.check_output", return_value="ok\n")

    # The parent's PATH is searched; argv, env and cwd pass through.
    child_path = r"C:\Elsewhere"
    assert (
        git_check_output(["git", "status"], cwd="repo", env={"PATH": child_path})
        == "ok"
    )
    assert check_output.call_args.args[0] == ["git", "status"]
    assert check_output.call_args.kwargs["executable"] == GIT
    assert check_output.call_args.kwargs["cwd"] == "repo"
    assert check_output.call_args.kwargs["env"]["PATH"] == child_path


@pytest.mark.kinda_slow
def test_git_check_output_error_shows_logical_command(monkeypatch) -> None:
    # Run a real child through the selected executable: Python, invoked as
    # "git", fails with the given status.
    monkeypatch.setattr(executable_lookup, "IS_WINDOWS", True)
    monkeypatch.setattr(
        executable_lookup, "search_windows_path", lambda *_args: sys.executable
    )
    url = "https://user:LEAK_ME@example.com/repo.git"
    with pytest.raises(SemgrepError) as error:
        git_check_output(["git", "-c", "import sys; sys.exit(3)", url])
    message = str(error.value)
    assert "exit code: 3" in message
    assert "'git -c import sys; sys.exit(3) https://<REDACTED>@example.com" in message
    assert sys.executable not in message
    assert "LEAK_ME" not in message


@pytest.mark.quick
def test_sub_check_output_fails_closed(monkeypatch, mocker) -> None:
    simulate_windows(monkeypatch, ".", {r".\git.exe"})
    check_output = mocker.patch("subprocess.check_output")
    with pytest.raises(FileNotFoundError) as not_found:
        sub_check_output(["git", "ls-files"])
    assert not_found.value.filename == "git"
    check_output.assert_not_called()


@pytest.mark.quick
@pytest.mark.parametrize(
    "find,expected",
    [
        ("entrypoint", r"C:\Tools\semgrep-core.exe"),
        ("semgrep-core", r"C:\Tools\semgrep-core.exe"),
    ],
)
def test_executable_path_fallback_ignores_cwd(monkeypatch, find, expected) -> None:
    # Importing entrypoint appends to PATH; let monkeypatch restore it.
    monkeypatch.setenv("PATH", os.environ.get("PATH", ""))
    entrypoint = importlib.import_module("semgrep.console_scripts.entrypoint")
    monkeypatch.setattr(entrypoint, "IS_WINDOWS", True)
    monkeypatch.setattr(semgrep_core, "IS_WINDOWS", True)

    def no_packaged_binary(*_args):
        raise FileNotFoundError

    # Windows CI has packaged binaries, which take precedence over PATH.
    monkeypatch.setattr(importlib.resources, "path", no_packaged_binary)
    files = {r".\semgrep-core.exe", r"C:\Tools\semgrep-core.exe"}
    simulate_windows(monkeypatch, r".;C:\Tools", files)

    if find == "entrypoint":
        assert entrypoint.find_semgrep_core_path() == expected
    else:
        assert semgrep_core.compute_executable_path(find) == expected

    # PATH has no fully qualified entry and sys.executable gives no directory.
    monkeypatch.setenv("PATH", ".")
    monkeypatch.setattr("sys.executable", "")
    if find == "entrypoint":
        with pytest.raises(entrypoint.CoreNotFound):
            entrypoint.find_semgrep_core_path()
    else:
        assert semgrep_core.compute_executable_path(find) is None


@pytest.mark.quick
@pytest.mark.parametrize("python_executable", ["", None])
def test_executable_path_without_interpreter_ignores_cwd(
    monkeypatch, mocker, tmp_path, python_executable
) -> None:
    mocker.patch.object(importlib.resources, "path", side_effect=FileNotFoundError)
    mocker.patch.object(semgrep_core, "find_executable", return_value=None)
    monkeypatch.chdir(tmp_path)
    core = "semgrep-core.exe" if semgrep_core.IS_WINDOWS else "semgrep-core"
    (tmp_path / core).touch()
    monkeypatch.setattr(sys, "executable", python_executable)

    assert semgrep_core.compute_executable_path("semgrep-core") is None
