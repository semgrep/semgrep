import click
import subprocess
from textwrap import dedent

from semgrep.commands.wrapper import handle_command_errors
from semgrep.util import abort
from semgrep.util import sub_check_output
from semgrep.verbose_logging import getLogger
from semgrep.error import SemgrepError

logger = getLogger(__name__)

# Check if `gh` command is installed
# If not, install it.
def install_github_cli():
    # Avoiding circular imports
    from semgrep.error import SemgrepError

    gh_exists = False
    try:
        sub_check_output(
            ["command", "-v", "gh"],
            timeout=4,
            encoding="utf-8",
            stderr=subprocess.STDOUT,
        ).rstrip()
        gh_exists = True
    except subprocess.CalledProcessError as e:
        if e.returncode == 1:
            # `gh` command not found
            pass
        else:
            raise e

    if gh_exists:
        return
    
    # NOTE: assume brew is already installed (for now)
    process = subprocess.Popen(
        ["brew", "install", "gh"],
        stdout=subprocess.PIPE, 
        stderr=subprocess.STDOUT,
        encoding="utf-8",
    )

    for line in process.stdout:
        logger.info(f"{line}")
    
    command = ["gh", "--version"]
    command_str = " ".join(command)

    try:
        sub_check_output(
            command,
            timeout=4,
            encoding="utf-8",
            stderr=subprocess.STDOUT,
        ).rstrip()
    except subprocess.CalledProcessError as e:
        raise SemgrepError(dedent(
            f"""
            Command failed with exit code: {e.returncode}
            -----
            Command failed with output:
            {e.stderr}

            Failed to run '{command_str}'. Possible reasons:

            - the gh binary is not available

            Try installing gh with `brew install gh` or
            visit https://cli.github.com for more information with installation.
            """
        ).strip())
    
def github_cli_login():
    logged_in = False
    try:
        sub_check_output(
            ["gh", "auth", "status"],
            timeout=4,
            encoding="utf-8",
            stderr=subprocess.STDOUT,
        ).rstrip()
        logged_in = True
    except subprocess.CalledProcessError as e:
        if e.returncode == 1:
            # e.g. `You are not logged into any GitHub hosts. Run gh auth login to authenticate.`
            pass
        else:
            raise e

    if logged_in:
        logger.debug(f"already logged in")
        return

    # NOTE: option to login via browser or token (for now, use browser)
    process = subprocess.Popen(
        ["gh", "auth", "login", "--web"],
        stdout=subprocess.PIPE, 
        stderr=subprocess.STDOUT,
        encoding="utf-8",
    )

    for line in process.stdout:
        logger.info(f"{line}")
    
    logger.info(f"done logging in")


@click.command()
@handle_command_errors
def install_semgrep_ci() -> None:
    """
    Install Semgrep CI in Github Actions

    Must be logged in to use; see `semgrep login`

    Visit https://semgrep.dev/deep-semgrep-beta for more information
    """
    logger.info("Installing Semgrep CI...")
    install_github_cli()    
    github_cli_login()

