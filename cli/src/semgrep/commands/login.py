#
# Copyright (c) 2021-2024 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import sys
import time
import uuid
from typing import NoReturn
from typing import Optional
from typing import Tuple

import click

from semgrep.app import auth
from semgrep.commands.wrapper import handle_command_errors
from semgrep.error import FATAL_EXIT_CODE
from semgrep.state import get_state
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def make_login_url() -> Tuple[uuid.UUID, str]:
    env = get_state().env
    session_id = uuid.uuid4()
    return (
        session_id,
        f"{env.semgrep_url}/login?cli-token={session_id}&docker={env.in_docker}&gha={env.in_gh_action}",
    )


@click.command()
# Cursor can call `semgrep login` and pop a window up for users to login without being in a
# TTY. So, we need to add a flag to the command to allow for this.
@click.option(
    "--override-tty",
    "override_tty",
    is_flag=True,
    help="Login from a non-interactive terminal. Used by agents calling to our MCP server.",
)
@handle_command_errors
def login(override_tty: bool) -> NoReturn:
    """
    Obtain and save credentials for semgrep.dev

    Looks for an semgrep.dev API token in the environment variable SEMGREP_APP_TOKEN.
    If not defined and running in a TTY, prompts interactively.
    Once token is found, saves it to global settings file
    """
    state = get_state()
    saved_login_token = auth._read_token_from_settings_file()
    if saved_login_token and saved_login_token != state.env.app_token:
        click.echo(
            f"API token already exists in {state.settings.path}. To login with a different token logout use `semgrep logout`"
        )
        sys.exit(FATAL_EXIT_CODE)

    # If the token is provided as an environment variable, save it to the settings file.
    if state.env.app_token is not None and len(state.env.app_token) > 0:
        if not save_token(state.env.app_token, echo_token=False):
            sys.exit(FATAL_EXIT_CODE)
        sys.exit(0)

    # If token doesn't already exist in the settings file or as an environment variable,
    # interactively prompt the user to supply it (if we are in a TTY).
    if not auth.is_a_tty() and not override_tty:
        click.echo(
            f"Error: semgrep login is an interactive command: run in an interactive terminal (or define SEMGREP_APP_TOKEN)",
            err=True,
        )
        sys.exit(FATAL_EXIT_CODE)

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
            headers={
                "User-Agent": str(state.app_session.user_agent),
                "X-Semgrep-Client-Id": str(
                    state.settings.get("anonymous_user_id") or ""
                ),
            },
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
                f"Unexpected failure from {state.env.semgrep_url}: status code {r.status_code}; please contact support@semgrep.com if this persists",
                err=True,
            )

        time.sleep(WAIT_BETWEEN_RETRY_IN_SEC)

    click.echo(
        f"Failed to login: please check your internet connection or contact support@semgrep.com",
        err=True,
    )
    sys.exit(FATAL_EXIT_CODE)


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
