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
import importlib.resources
import os
import shutil
import sys
from pathlib import Path
from typing import Optional

from semgrep.util import IS_WINDOWS
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

VERSION_STAMP_FILENAME = "pro-installed-by.txt"


def compute_executable_path(exec_name: str) -> Optional[str]:
    """
    Determine full executable path if full path is needed to run it.

    Return None if no executable found
    """
    if IS_WINDOWS:
        exec_name += ".exe"

    # First, try packaged binaries
    try:
        # TODO: .path() is deprecated, use .files() instead to avoid
        # annoying deprecation notice when running tests
        with importlib.resources.path("semgrep.bin", exec_name) as path:
            if path.is_file():
                return str(path)
    except FileNotFoundError as e:
        logger.debug(f"Failed to open resource {exec_name}: {e}.")

    # Second, try system binaries in PATH.
    #
    # Environment variables, including PATH, are not inherited by the pytest
    # jobs (at least by default), so this won't work when running pytest
    # tests.
    #
    which_exec = shutil.which(exec_name)
    if which_exec is not None:
        return which_exec

    # Third, look for something in the same dir as the Python interpreter
    relative_path = os.path.join(os.path.dirname(sys.executable), exec_name)
    if os.path.isfile(relative_path):
        return relative_path

    return None


class SemgrepCore:
    # Path to the semgrep-core executable, or None if we could not find either
    # or have not looked yet.
    _SEMGREP_PATH_: Optional[str] = None

    _PRO_PATH_: Optional[str] = None

    @classmethod
    def executable_path(cls) -> str:
        """
        Return the path to the semgrep stand-alone executable binary,
        *not* the Python module.  This is intended for unusual cases
        that the module plumbing is not set up to handle.

        Raise Exception if the executable is not found.
        """
        # This method does not bother to cache its result since it is
        # expected to not be a part of any critical path.
        ret = compute_executable_path("semgrep-core")
        if ret is None:
            raise Exception("Could not locate semgrep-core binary")
        return ret

    @classmethod
    def path(cls) -> Path:
        """
        Return the path to the semgrep stand-alone program.  Raise Exception if
        not found.
        """
        if cls._SEMGREP_PATH_ is None:
            cls._SEMGREP_PATH_ = cls.executable_path()

        return Path(cls._SEMGREP_PATH_)

    @classmethod
    def pro_path(cls) -> Optional[Path]:
        if cls._PRO_PATH_ is None:
            cls._PRO_PATH_ = compute_executable_path("semgrep-core-proprietary")

        return Path(cls._PRO_PATH_) if cls._PRO_PATH_ is not None else None

    @classmethod
    def pro_version_stamp_path(cls) -> Path:
        return cls.path().parent / VERSION_STAMP_FILENAME
