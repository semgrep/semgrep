import os
import shutil
import sys
from typing import Optional

import pkg_resources


def compute_executable_path(exec_name: str) -> str:
    """Determine full executable path if full path is needed to run it."""
    # First, try packaged binaries
    pkg_exec = pkg_resources.resource_filename("semgrep.bin", exec_name)
    if os.path.isfile(pkg_exec):
        return pkg_exec

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

    raise Exception(f"Could not locate '{exec_name}' binary")


class SemgrepCore:
    _SEMGREP_PATH_: Optional[str] = None

    @classmethod
    def path(cls) -> str:
        if cls._SEMGREP_PATH_ is None:
            cls._SEMGREP_PATH_ = compute_executable_path("semgrep-core")
        return cls._SEMGREP_PATH_
