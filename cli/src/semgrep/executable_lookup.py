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
"""Executable lookup that never selects a program from the current directory.

On Windows, CreateProcess (used by subprocess and asyncio) and shutil.which
resolve a bare program name by consulting the current directory before PATH,
so a scanned checkout could supply e.g. git.exe. Here, a bare name is instead
searched for only in the fully qualified entries of the parent's PATH, and
lookup fails rather than falling back to the bare name. Callers pass the
result as subprocess's `executable`, which Windows runs without searching,
while the logical argv is still what errors and the child's argv[0] show.
Programs with a directory component are left alone, as is everything on other
platforms.

This module is imported by console_scripts/entrypoint.py and must stay
lightweight: standard library only.
"""
import errno
import ntpath
import os
import platform
import shutil
from pathlib import PureWindowsPath
from typing import Callable
from typing import List
from typing import Optional
from typing import Sequence
from typing import Union

IS_WINDOWS = platform.system() == "Windows"


def is_fully_qualified_windows_path(path: str) -> bool:
    """True for "C:\\dir" and UNC paths such as "\\\\server\\share";
    False for drive-relative "C:dir", root-relative "\\dir" and relative
    paths. PureWindowsPath.is_absolute alone also accepts rootless UNC and
    device paths on some Python versions.
    """
    p = PureWindowsPath(path)
    return bool(p.drive and p.root)


def _is_bare_windows_name(name: str) -> bool:
    return bool(name) and not ntpath.dirname(name) and ":" not in name


def split_windows_path_var(path: str) -> List[str]:
    """Split a Windows PATH value on ';' outside double quotes, dropping the
    quotes."""
    entries: List[str] = []
    cur: List[str] = []
    in_quotes = False
    for c in path:
        if c == '"':
            in_quotes = not in_quotes
        elif c == ";" and not in_quotes:
            entries.append("".join(cur))
            cur = []
        else:
            cur.append(c)
    entries.append("".join(cur))
    return entries


def search_windows_path(
    name: str,
    path: Optional[str],
    is_file: Optional[Callable[[str], bool]] = None,
) -> Optional[str]:
    """The first match for the bare program [name] in the fully qualified
    entries of the PATH value [path], or None.

    Like CreateProcess, ".exe" is appended when [name] has no extension.
    """
    if is_file is None:
        is_file = os.path.isfile
    file = name if "." in name else name + ".exe"
    for entry in split_windows_path_var(path or ""):
        if is_fully_qualified_windows_path(entry):
            candidate = ntpath.join(entry, file)
            if is_file(candidate):
                return candidate
    return None


def find_executable(name: str) -> Optional[str]:
    """Replacement for shutil.which(name) that, on Windows, ignores the
    current directory and PATH entries that are not fully qualified.

    On Windows, names get CreateProcess's suffix handling, not PATHEXT's, so
    the result is what resolve_executable would select.
    """
    if not IS_WINDOWS:
        return shutil.which(name)
    if not _is_bare_windows_name(name):
        return name if name and os.path.isfile(name) else None
    return search_windows_path(name, os.environ.get("PATH"))


def resolve_executable(
    command: Sequence[Union[str, "os.PathLike[str]"]]
) -> Optional[str]:
    """The `executable` to pass to subprocess for [command].

    None, so that subprocess behaves as usual, except on Windows when the
    program is a bare name. Raises FileNotFoundError, naming the logical
    program, if that name is not found. Lookup uses this process's PATH, as
    CreateProcess does, even when the child is given a different environment.
    """
    if not IS_WINDOWS or not command:
        return None
    name = os.fspath(command[0])
    if not _is_bare_windows_name(name):
        return None
    found = search_windows_path(name, os.environ.get("PATH"))
    if found is None:
        raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), name)
    return found
