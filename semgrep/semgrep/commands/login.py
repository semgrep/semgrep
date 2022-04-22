import os
import sys
import time
import uuid
from typing import NoReturn
from typing import Optional
from typing import Tuple

import click

from semgrep.app import app_session
from semgrep.app import auth
from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import IN_DOCKER
from semgrep.constants import IN_GH_ACTION
from semgrep.constants import SEMGREP_URL
from semgrep.error import FATAL_EXIT_CODE
from semgrep.settings import SETTINGS
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def make_login_url() -> Tuple[uuid.UUID, str]:
    session_id = uuid.uuid4()
    return (
        session_id,
        f"{SEMGREP_URL}/login?cli-token={session_id}&docker={IN_DOCKER}&gha={IN_GH_ACTION}",
    )


@click.command()
@handle_command_errors
def login() -> NoReturn:
    """
    Obtain and save credentials for semgrep.dev

    Looks for an semgrep.dev API token in the environment variable SEMGREP_API_TOKEN_SETTINGS_KEY.
    If not defined and running in a TTY, prompts interactively.
    Once token is found, saves it to global settings file
    """
    saved_login_token = auth._read_token_from_settings_file()
    if saved_login_token:
        click.echo(
            f"API token already exists in {SETTINGS.get_path_to_settings()}. To login with a different token logout use `semgrep logout`"
        )
        sys.exit(FATAL_EXIT_CODE)

    # If the token is provided as an environment variable, save it to the settings file.
    env_var_token = os.environ.get(auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME)
    if env_var_token is not None and len(env_var_token) > 0:
        if not save_token(env_var_token, echo_token=False):
            sys.exit(FATAL_EXIT_CODE)
        sys.exit(0)

    # If token doesn't already exist in the settings file or as an environment variable,
    # interactively prompt the user to supply it (if we are in a TTY).
    if not auth.is_a_tty():
        click.echo(
            f"Error: semgrep login is an interactive command: run in an interactive terminal (or define {auth.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME})",
            err=True,
        )
        sys.exit(FATAL_EXIT_CODE)

    session_id, url = make_login_url()
    click.echo(
        "Login enables additional proprietary Semgrep Registry rules and running custom policies from Semgrep App."
    )
    click.echo(f"Login at: {url}")
    click.echo(
        "\nOnce you've logged in, return here and you'll be ready to start using new Semgrep rules."
    )
    WAIT_BETWEEN_RETRY_IN_SEC = 6  # So every 10 retries is a minute
    MAX_RETRIES = 30  # Give users 3 minutes to log in / open link

    for _ in range(MAX_RETRIES):
        r = app_session.post(
            f"{SEMGREP_URL}/api/agent/tokens/requests",
            json={"token_request_key": str(session_id)},
        )
        if r.status_code == 200:
            as_json = r.json()
            if save_token(as_json.get("token"), echo_token=True):
                sys.exit(0)
            else:
                sys.exit(FATAL_EXIT_CODE)
        elif r.status_code != 404:
            click.echo(
                f"Unexpected failure from {SEMGREP_URL}: status code {r.status_code}; please contact support@r2c.dev if this persists",
                err=True,
            )

        time.sleep(WAIT_BETWEEN_RETRY_IN_SEC)

    click.echo(
        f"Failed to login: please check your internet connection or contact support@r2c.dev",
        err=True,
    )
    sys.exit(FATAL_EXIT_CODE)


def save_token(login_token: Optional[str], echo_token: bool) -> bool:
    if login_token is not None and auth.is_valid_token(login_token):
        auth.set_token(login_token)
        click.echo(
            f"Saved login token\n\n\t{login_token if echo_token else '<redacted>'}\n\nin {SETTINGS.get_path_to_settings()}."
        )
        click.echo(
            f"Note: You can always generate more tokens at {SEMGREP_URL}/orgs/-/settings/tokens"
        )
        return True
    else:
        click.echo("Login token is not valid. Please try again.", err=True)
        return False


@click.command()
@handle_command_errors
def logout() -> None:
    """
    Remove locally stored credentials to semgrep.dev
    """
    auth.delete_token()
    click.echo("Logged out (log back in with `semgrep login`)")
