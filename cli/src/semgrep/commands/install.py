import os
import platform
import shutil
import stat
import subprocess
import sys
from pathlib import Path
from typing import Tuple

import click
from tqdm import tqdm

from semgrep.commands.wrapper import handle_command_errors
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.semgrep_core import SemgrepCore
from semgrep.state import get_state
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def determine_semgrep_pro_path() -> Tuple[Path, Path]:
    core_path = SemgrepCore.path()
    if core_path is None:
        logger.info(
            "Could not find `semgrep-core` executable so not sure where to install DeepSemgrep"
        )
        logger.info("There is something wrong with your semgrep installtation")
        sys.exit(FATAL_EXIT_CODE)

    deep_semgrep_path = Path(core_path).parent / "deep-semgrep"
    semgrep_pro_path = Path(core_path).parent / "semgrep-core-proprietary"
    return (semgrep_pro_path, deep_semgrep_path)


def run_install_semgrep_pro() -> None:
    state = get_state()
    state.terminal.configure(verbose=False, debug=False, quiet=False, force_color=False)

    (semgrep_pro_path, deep_semgrep_path) = determine_semgrep_pro_path()

    # TODO This is a temporary solution to help offline users
    logger.info(f"Semgrep Pro Engine will be installed in {semgrep_pro_path}")

    if deep_semgrep_path.exists():
        logger.warning(
            f"""You have an old DeepSemgrep binary installed in {deep_semgrep_path}.
DeepSemgrep has been renamed to Semgrep PRO, which we will proceed to install.
Please delete {deep_semgrep_path} manually to make this warning disappear!
"""
        )
    if semgrep_pro_path.exists():
        logger.info(f"Overwriting Semgrep Pro Engine already installed!")

    if state.app_session.token is None:
        logger.info("run `semgrep login` before using `semgrep install`")
        sys.exit(INVALID_API_KEY_EXIT_CODE)

    if sys.platform.startswith("darwin"):
        # arm64 is possible. Dunno if other arms are, so let's just check a prefix.
        if platform.machine().startswith("arm"):
            platform_kind = "osx-arm64"
        else:
            platform_kind = "osx-x86_64"
    elif sys.platform.startswith("linux"):
        platform_kind = "manylinux"
    else:
        platform_kind = "manylinux"
        logger.info(
            "Running on potentially unsupported platform. Installing linux compatible binary"
        )

    url = f"{state.env.semgrep_url}/api/agent/deployments/deepbinary/{platform_kind}"

    with state.app_session.get(url, timeout=60, stream=True) as r:
        if r.status_code == 401:
            logger.info(
                "API token not valid. Try to run `semgrep logout` and `semgrep login` again."
            )
            sys.exit(INVALID_API_KEY_EXIT_CODE)
        if r.status_code == 403:
            logger.warning(
                "Logged in deployment does not have access to Semgrep Pro Engine beta"
            )
            # FIXME: Needs to be updated before launch Feb 2023
            logger.warning(
                "Visit https://semgrep.dev/deep-semgrep-beta for more information."
            )
            sys.exit(FATAL_EXIT_CODE)
        r.raise_for_status()

        file_size = int(r.headers.get("Content-Length", 0))

        with open(semgrep_pro_path, "wb") as f:
            with tqdm.wrapattr(r.raw, "read", total=file_size) as r_raw:
                shutil.copyfileobj(r_raw, f)

    # THINK: Do we need to give exec permissions to everybody? Can this be a security risk?
    #        The binary should not have setuid or setgid rights, so letting others
    #        execute it should not be a problem.
    # nosemgrep: python.lang.security.audit.insecure-file-permissions.insecure-file-permissions
    os.chmod(
        semgrep_pro_path,
        os.stat(semgrep_pro_path).st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH,
    )
    logger.info(f"Installed Semgrep Pro to {semgrep_pro_path}")

    version = sub_check_output(
        [str(semgrep_pro_path), "-pro_version"],
        timeout=10,
        encoding="utf-8",
        stderr=subprocess.STDOUT,
    ).rstrip()
    logger.info(f"Semgrep Pro Engine Version Info: ({version})")


@click.command(hidden=True)
@handle_command_errors
def install_semgrep_pro() -> None:
    """
    Install the Semgrep Pro Engine binary (Experimental)

    The binary is installed in the same directory that semgrep-core
    is installed in.

    Must be logged in and have access to Semgrep Pro Engine beta
    Visit https://semgrep.dev/deep-semgrep-beta for more information
    """
    run_install_semgrep_pro()


# DEPRECATED: To be removed by Feb 2023 launch
@click.command(hidden=True)
@handle_command_errors
def install_deep_semgrep() -> None:
    """
    This no longer installs the DeepSemgrep binary, but it will suggest
    to you the real command that does.
    """
    logger.info(f"This command no longer installs the DeepSemgrep binary.")
    logger.info(
        f"Instead, try installing the Semgrep Pro Engine via `semgrep install-semgrep-pro`."
    )

    sys.exit(FATAL_EXIT_CODE)
