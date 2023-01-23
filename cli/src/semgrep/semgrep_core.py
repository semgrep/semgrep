import importlib.resources
import os
import shutil
import sys
import types
from typing import Optional

from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def compute_executable_path(exec_name: str) -> Optional[str]:
    """
    Determine full executable path if full path is needed to run it.

    Return None if no executable found
    """
    # First, try packaged binaries
    try:
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
    # Path to either the bridge module .so file or the semgrep-core
    # executable, or None if we could not find either or have not looked
    # yet.
    _SEMGREP_PATH_: Optional[str] = None

    # DEPRECATED: To be removed by Feb 2023 launch
    _DEEP_PATH_: Optional[str] = None

    _PRO_PATH_: Optional[str] = None

    # Reference to the bridge module if we are using it.
    _bridge_module: Optional[types.ModuleType] = None

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
    def path(cls) -> str:
        """
        Return the path to the semgrep binary, either the Python module
        or the stand-alone program.  Raise Exception if neither is
        found.
        """
        if cls._SEMGREP_PATH_ is None:
            # Try the module first if enabled.
            use_bridge = os.getenv("SEMGREP_USE_BRIDGE")
            if use_bridge:
                bridge_path = compute_executable_path("semgrep_bridge_python.so")
                if bridge_path:
                    # Temporarily put the path to the .so in front.
                    orig_sys_path = sys.path
                    sys.path = sys.path.copy()
                    sys.path.insert(0, os.path.dirname(bridge_path))

                    try:
                        import semgrep_bridge_python

                        cls._bridge_module = semgrep_bridge_python
                        cls._SEMGREP_PATH_ = bridge_path
                        logger.debug(f"Bridge module imported: {bridge_path}.")
                    except BaseException as e:
                        logger.warn(
                            f"Failed to load bridge module {bridge_path}: "
                            f"{e}.  Will fall back on executable."
                        )

                    # Restore the search path.
                    sys.path = orig_sys_path

            # Try the executable next.
            if cls._SEMGREP_PATH_ is None:
                # For use during testing, this value requires that the
                # bridge be used, disabling fallback to the executable.
                if use_bridge == "require":
                    raise Exception(
                        "Failed to load bridge, and SEMGREP_USE_BRIDGE is 'require'."
                    )

                cls._SEMGREP_PATH_ = cls.executable_path()

        return cls._SEMGREP_PATH_

    @classmethod
    def get_bridge_module(cls) -> types.ModuleType:
        """
        Return a reference to the bridge module.

        Requires 'using_bridge_module()'.
        """
        assert cls._bridge_module is not None
        return cls._bridge_module

    @classmethod
    def using_bridge_module(cls) -> bool:
        """
        Return true if we are using the bridge module.

        This should be called after path(), which decides whether to use
        the module.
        """
        return cls._bridge_module is not None

    # DEPRECATED: To be removed by Feb 2023 launch
    @classmethod
    def deep_path(cls) -> Optional[str]:
        if cls._DEEP_PATH_ is None:
            cls._DEEP_PATH_ = compute_executable_path("deep-semgrep")
        return cls._DEEP_PATH_

    @classmethod
    def pro_path(cls) -> Optional[str]:
        if cls._PRO_PATH_ is None:
            cls._PRO_PATH_ = compute_executable_path("semgrep-core-proprietary")
        return cls._PRO_PATH_
