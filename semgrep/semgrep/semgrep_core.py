import importlib.resources
import os
import shutil
import sys
from typing import Optional


def compute_executable_path(exec_name: str) -> Optional[str]:
    """
    Determine full executable path if full path is needed to run it.

    Return None if no executable found
    """
    # First, try packaged binaries
    with importlib.resources.path("semgrep.bin", exec_name) as path:
        if path.is_file():
            return str(path)

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
    _SEMGREP_PATH_: Optional[str] = None
    _DEEP_PATH_: Optional[str] = None

    @classmethod
    def path(cls) -> str:
        if cls._SEMGREP_PATH_ is None:
            cls._SEMGREP_PATH_ = compute_executable_path("semgrep-core")
            if cls._SEMGREP_PATH_ is None:
                raise Exception("Could not locate semgrep-core binary")

        return cls._SEMGREP_PATH_

    @classmethod
    def deep_path(cls) -> Optional[str]:
        if cls._DEEP_PATH_ is None:
            cls._DEEP_PATH_ = compute_executable_path("deep-semgrep")
        return cls._DEEP_PATH_
