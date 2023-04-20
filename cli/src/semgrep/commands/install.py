import os
import platform
import shutil
import stat
import subprocess
import sys
from pathlib import Path

import click
from rich.progress import BarColumn
from rich.progress import DownloadColumn
from rich.progress import Progress
from rich.progress import TextColumn
from rich.progress import TimeRemainingColumn
from rich.progress import TransferSpeedColumn

from semgrep import __VERSION__
from semgrep.commands.wrapper import handle_command_errors
from semgrep.console import console
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.semgrep_core import SemgrepCore
from semgrep.state import get_state
from semgrep.util import abort
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def determine_semgrep_pro_path() -> Path:
    core_path = SemgrepCore.path()
    if core_path is None:
        logger.info(
            "Could not find `semgrep-core` executable so not sure where to install DeepSemgrep"
        )
        logger.info("There is something wrong with your semgrep installtation")
        sys.exit(FATAL_EXIT_CODE)

    semgrep_pro_path = Path(core_path).parent / "semgrep-core-proprietary"
    return semgrep_pro_path


def run_install_semgrep_pro() -> None:
    state = get_state()
    state.terminal.configure(verbose=False, debug=False, quiet=False, force_color=False)

    semgrep_pro_path = determine_semgrep_pro_path()

    # TODO This is a temporary solution to help offline users
    logger.info(f"Semgrep Pro Engine will be installed in {semgrep_pro_path}")

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

    url = f"{state.env.semgrep_url}/api/agent/deployments/deepbinary/{platform_kind}?version={__VERSION__}"

    # Download the binary into a temporary location, check it, then install it.
    # This should prevent bad installations.

    semgrep_pro_path_tmp = semgrep_pro_path.with_suffix(".tmp_download")

    with state.app_session.get(url, timeout=180, stream=True) as r:
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

        # Make sure no such binary exists. We have had weird situations when the
        # downloaded binary was corrupted, and overwriting it did not fix it, but
        # it was necessary to `rm -f` it.
        if semgrep_pro_path_tmp.exists():
            semgrep_pro_path_tmp.unlink()

        file_size = int(r.headers.get("Content-Length", 0))

        with Progress(
            TextColumn("{task.description}"),
            BarColumn(),
            DownloadColumn(),
            TransferSpeedColumn(),
            TimeRemainingColumn(),
            console=console,
        ) as progress, semgrep_pro_path_tmp.open("wb") as f, progress.wrap_file(
            r.raw, total=file_size, description="Downloading..."
        ) as r_raw:
            shutil.copyfileobj(r_raw, f)

    # THINK: Do we need to give exec permissions to everybody? Can this be a security risk?
    #        The binary should not have setuid or setgid rights, so letting others
    #        execute it should not be a problem.
    # nosemgrep: tests.precommit_dogfooding.python.lang.security.audit.insecure-file-permissions.insecure-file-permissions
    os.chmod(
        semgrep_pro_path_tmp,
        os.stat(semgrep_pro_path_tmp).st_mode
        | stat.S_IEXEC
        | stat.S_IXGRP
        | stat.S_IXOTH,
    )

    # Get Pro version, it serves as a simple check that the binary works
    try:
        version = sub_check_output(
            [str(semgrep_pro_path_tmp), "-pro_version"],
            timeout=10,
            encoding="utf-8",
            stderr=subprocess.STDOUT,
        ).rstrip()
    except subprocess.CalledProcessError:
        if semgrep_pro_path_tmp.exists():
            semgrep_pro_path_tmp.unlink()
        abort(
            "Downloaded binary failed version check, try again or contact support@r2c.dev"
        )

    # Version check worked so we now install the binary
    if semgrep_pro_path.exists():
        semgrep_pro_path.unlink()
    semgrep_pro_path_tmp.rename(semgrep_pro_path)
    logger.info(f"Successfully installed Semgrep Pro Engine (version {version})!")


@click.command()
@handle_command_errors
def install_semgrep_pro() -> None:
    """
    Install the Semgrep Pro Engine

    The binary is installed in the same directory that semgrep-core
    is installed in.

    Must be logged in and have access to Semgrep Pro Engine beta
    Visit https://semgrep.dev/deep-semgrep-beta for more information
    """
    run_install_semgrep_pro()
