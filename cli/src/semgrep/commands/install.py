import os
import shutil
import stat
import subprocess
import sys
from pathlib import Path

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


@click.command(hidden=True)
@handle_command_errors
def install_deep_semgrep() -> None:
    """
    Install the DeepSemgrep binary (Experimental)

    The binary is installed in the same directory that semgrep-core
    is installed in. Does not reinstall if existing binary already exists.

    Must be logged in and have access to DeepSemgrep beta
    Visit https://semgrep.dev/deep-semgrep-beta for more information
    """
    state = get_state()
    state.terminal.configure(verbose=False, debug=False, quiet=False, force_color=False)

    core_path = SemgrepCore.path()
    if core_path is None:
        logger.info(
            "Could not find `semgrep-core` executable so not sure where to install DeepSemgrep"
        )
        logger.info("There is something wrong with your semgrep installtation")
        sys.exit(FATAL_EXIT_CODE)

    deep_semgrep_path = Path(core_path).parent / "deep-semgrep"
    if deep_semgrep_path.exists():
        logger.info(
            f"Overwriting DeepSemgrep binary already installed in {deep_semgrep_path}"
        )

    if state.app_session.token is None:
        logger.info("run `semgrep login` before using `semgrep install`")
        sys.exit(INVALID_API_KEY_EXIT_CODE)

    if sys.platform.startswith("darwin"):
        platform = "osx"
    elif sys.platform.startswith("linux"):
        platform = "manylinux"
    else:
        platform = "manylinux"
        logger.info(
            "Running on potentially unsupported platform. Installing linux compatible binary"
        )

    url = f"{state.env.semgrep_url}/api/agent/deployments/deepbinary/{platform}"

    with state.app_session.get(url, timeout=60, stream=True) as r:
        if r.status_code == 401:
            logger.info(
                "API token not valid. Try to run `semgrep logout` and `semgrep login` again."
            )
            sys.exit(INVALID_API_KEY_EXIT_CODE)
        if r.status_code == 403:
            logger.info("Logged in deployment does not have access to DeepSemgrep beta")
            logger.info(
                "Visit https://semgrep.dev/deep-semgrep-beta for more information."
            )
            sys.exit(FATAL_EXIT_CODE)
        r.raise_for_status()

        file_size = int(r.headers.get("Content-Length", 0))

        with open(deep_semgrep_path, "wb") as f:
            with tqdm.wrapattr(r.raw, "read", total=file_size) as r_raw:
                shutil.copyfileobj(r_raw, f)

    os.chmod(
        deep_semgrep_path,
        os.stat(deep_semgrep_path).st_mode | stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH,
    )
    logger.info(f"Installed deepsemgrep to {deep_semgrep_path}")

    version = sub_check_output(
        [str(deep_semgrep_path), "--version"],
        timeout=10,
        encoding="utf-8",
        stderr=subprocess.DEVNULL,
    ).rstrip()
    logger.info(f"DeepSemgrep Version Info: ({version})")
