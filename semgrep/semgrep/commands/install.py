import shutil
import sys
from pathlib import Path

import click
import requests
from tqdm import tqdm

from semgrep.commands.login import Authentication
from semgrep.constants import SEMGREP_URL
from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import INVALID_API_KEY_EXIT_CODE
from semgrep.semgrep_core import SemgrepCore
from semgrep.util import set_flags
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@click.group()
def install() -> None:
    """
    Install specific semgrep dependencies (Experimental)
    """
    set_flags(verbose=False, debug=False, quiet=False, force_color=False)


@click.command()
@click.option(
    "--force", is_flag=True, help="Override existing installation of DeepSemgrep"
)
def deepsemgrep(force: bool) -> None:
    """
    Install the DeepSemgrep binary.

    The binary is installed in the same directory that semgrep-core
    is installed in. Does not reinstall if existing binary already exists.

    Must be logged in and have access to DeepSemgrep beta
    Visit https://semgrep.dev/deep-semgrep-beta for more information
    """
    core_path = SemgrepCore.path()
    if core_path is None:
        logger.info(
            "Could not find `semgrep-core` executable so not sure where to install DeepSemgrep"
        )
        logger.info("There is something wrong with your semgrep installtation")
        sys.exit(FATAL_EXIT_CODE)

    deep_semgrep_path = Path(core_path).parent / "deep-semgrep"
    if deep_semgrep_path.exists():
        logger.info(f"DeepSemgrep binary already installed in {deep_semgrep_path}. ")
        if not force:
            logger.info("Rerun with `--force` to override existing binary")
            sys.exit(FATAL_EXIT_CODE)
        else:
            logger.info(f"Reinstalling DeepSemgrep since `--force` passed.")

    token = Authentication.get_token()
    if token is None:
        logger.info("run `semgrep login` before using `semgrep install`")
        sys.exit(INVALID_API_KEY_EXIT_CODE)

    headers = {"User-Agent": SEMGREP_USER_AGENT, "Authorization": f"Bearer {token}"}

    with requests.get(
        f"{SEMGREP_URL}/api/agent/deployments/deepbinary",
        headers=headers,
        timeout=60,
        stream=True,
    ) as r:
        # TODO if does not have access point to beta info page
        r.raise_for_status()

        file_size = int(r.headers.get("Content-Length", 0))

        with open(deep_semgrep_path, "wb") as f:
            with tqdm.wrapattr(r.raw, "read", total=file_size) as r_raw:
                shutil.copyfileobj(r_raw, f)

    logger.info(f"Installed deepsemgrep to {deep_semgrep_path}")


install.add_command(deepsemgrep)
