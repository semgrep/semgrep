import sys
import time
import uuid
from typing import Optional
from typing import Tuple

import click

from semgrep.app import auth
from semgrep.commands.wrapper import AdvancedCommand
from semgrep.commands.wrapper import handle_command_errors
from semgrep.constants import FATAL_EXIT_CODE
from semgrep.state import get_state
from semgrep.util import abort
from semgrep.util import warn
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def make_login_url() -> Tuple[uuid.UUID, str]:
    env = get_state().env
    session_id = uuid.uuid4()
    return (
        session_id,
        f"{env.semgrep_url}/login?cli-token={session_id}&docker={env.in_docker}&gha={env.in_gh_action}",
    )


@click.command(cls=AdvancedCommand)
@handle_command_errors
def login() -> None:
    """
    Obtain and save credentials for semgrep.dev

    Looks for an semgrep.dev API token in the environment variable SEMGREP_APP_TOKEN.
    If not defined and running in a TTY, prompts interactively.
    Once token is found, saves it to global settings file
    """
    state = get_state()
    saved_login_token = auth._read_token_from_settings_file()
    if saved_login_token:
        abort(
            f"API token already exists in {state.settings.path}. To login with a different token logout use `semgrep logout`"
        )

    # If the token is provided as an environment variable, save it to the settings file.
    if state.env.app_token is not None and len(state.env.app_token) > 0:
        if not save_token(state.env.app_token, echo_token=False):
            sys.exit(FATAL_EXIT_CODE)
        sys.exit(0)

    # If token doesn't already exist in the settings file or as an environment variable,
    # interactively prompt the user to supply it (if we are in a TTY).
    if not auth.is_a_tty():
        abort(
            f"Error: semgrep login is an interactive command: run in an interactive terminal (or define SEMGREP_APP_TOKEN)",
        )

    session_id, url = make_login_url()
    click.echo(
        "Login enables additional proprietary Semgrep Registry rules and running custom policies from Semgrep Cloud Platform."
    )
    click.echo(f"Opening login at: {url}")
    click.launch(url)
    click.echo(
        "\nOnce you've logged in, return here and you'll be ready to start using new Semgrep rules."
    )
    WAIT_BETWEEN_RETRY_IN_SEC = 6  # So every 10 retries is a minute
    MAX_RETRIES = 30  # Give users 3 minutes to log in / open link

    for _ in range(MAX_RETRIES):
        r = state.app_session.post(
            f"{state.env.semgrep_url}/api/agent/tokens/requests",
            json={"token_request_key": str(session_id)},
        )
        if r.status_code == 200:
            as_json = r.json()
            click.echo(f"Token: {as_json}")
            if save_token(as_json.get("token"), echo_token=True):
                sys.exit(0)
            else:
                abort("Failed to save auth token")
        elif r.status_code != 404:
            warn(
                f"Unexpected failure from {state.env.semgrep_url}: status code {r.status_code}; please contact support@semgrep.com if this persists",
            )

        time.sleep(WAIT_BETWEEN_RETRY_IN_SEC)

    abort(
        f"Failed to login: please check your internet connection or contact support@semgrep.com",
    )


def save_token(login_token: Optional[str], echo_token: bool) -> bool:
    state = get_state()
    if login_token is not None and auth.get_deployment_from_token(login_token):
        auth.set_token(login_token)
        click.echo(
            f"Saved login token\n\n\t{login_token if echo_token else '<redacted>'}\n\nin {state.settings.path}."
        )
        click.echo(
            f"Note: You can always generate more tokens at {state.env.semgrep_url}/orgs/-/settings/tokens"
        )
        return True
    else:
        click.echo("Login token is not valid. Please try again.", err=True)
        return False


@click.command(cls=AdvancedCommand)
@handle_command_errors
def logout() -> None:
    """
    Remove locally stored credentials to semgrep.dev
    """
    auth.delete_token()
    click.echo("Logged out (log back in with `semgrep login`)")
