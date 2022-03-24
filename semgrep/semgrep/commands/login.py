import os
import sys
import time
from typing import Optional
from typing import Tuple

import click

from semgrep.constants import IN_DOCKER
from semgrep.constants import IN_GH_ACTION
from semgrep.constants import SEMGREP_URL
from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.settings import SETTINGS
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)
import uuid


def make_login_url() -> Tuple[uuid.UUID, str]:
    session_id = uuid.uuid4()
    return (
        session_id,
        f"{SEMGREP_URL}login?cli-token={session_id}&docker={IN_DOCKER}&gha={IN_GH_ACTION}",
    )


@click.command()
def login() -> None:
    """
    Looks for an semgrep.dev API token in the environment variable SEMGREP_API_TOKEN_SETTINGS_KEY.
    If not defined and running in a TTY, prompts interactively.
    Once token is found, saves it to global settings file
    """
    saved_login_token = Authentication._read_token_from_settings_file()
    if saved_login_token:
        click.echo(
            f"API token already exists in {SETTINGS.get_path_to_settings()}. To login with a different token logout use `semgrep logout`"
        )
        sys.exit(1)

    # If the token is provided as an environment variable, save it to the settings file.
    env_var_token = os.environ.get(Authentication.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME)
    if env_var_token is not None and len(env_var_token) > 0:
        if not save_token(env_var_token, echo_token=False):
            sys.exit(1)
        sys.exit(0)

    # If token doesn't already exist in the settings file or as an environment variable,
    # interactively prompt the user to supply it (if we are in a TTY).
    if Authentication.is_a_tty():
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
        import requests

        for _ in range(MAX_RETRIES):
            headers = {"User-Agent": SEMGREP_USER_AGENT}
            r = requests.post(
                f"{SEMGREP_URL}api/agent/tokens/requests",
                json={"token_request_key": str(session_id)},
                timeout=10,
                headers=headers,
            )
            if r.status_code == 200:
                as_json = r.json()
                if save_token(as_json.get("token"), echo_token=True):
                    sys.exit(0)
                else:
                    sys.exit(1)
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
        sys.exit(1)

    else:
        click.echo(
            f"Error: semgrep login is an interactive command: run in an interactive terminal (or define {Authentication.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME})",
            err=True,
        )
        sys.exit(1)


def save_token(login_token: Optional[str], echo_token: bool) -> bool:
    if login_token is not None and Authentication.is_valid_token(login_token):
        Authentication.set_token(login_token)
        click.echo(
            f"Saved login token\n\n\t{login_token if echo_token else '<redacted>'}\n\nin {SETTINGS.get_path_to_settings()}."
        )
        click.echo(
            f"Note: You can always generate more tokens at {SEMGREP_URL}orgs/-/settings/tokens"
        )
        return True
    else:
        click.echo("Login token is not valid. Please try again.", err=True)
        return False


@click.command()
def logout() -> None:
    """
    Remove all authentication tokens from global settings file
    """
    Authentication.delete_token()
    click.echo("Logged out (log back in with `semgrep login`)")


class Authentication:
    SEMGREP_LOGIN_TOKEN_ENVVAR_NAME = "SEMGREP_APP_TOKEN"
    SEMGREP_API_TOKEN_SETTINGS_KEY = "api_token"

    @staticmethod
    def is_valid_token(token: str) -> bool:
        """
        Returns true if token is valid
        """
        import requests

        headers = {"User-Agent": SEMGREP_USER_AGENT, "Authorization": f"Bearer {token}"}
        r = requests.get(
            f"{SEMGREP_URL}api/agent/deployments", timeout=10, headers=headers
        )
        return r.ok

    @staticmethod
    def get_deployment_id() -> Optional[int]:
        """
        Returns the deployment_id attached to an api_token as int

        Returns None if api_token is invalid/doesn't have associated deployment
        """
        import requests

        token = Authentication.get_token()

        headers = {"User-Agent": SEMGREP_USER_AGENT, "Authorization": f"Bearer {token}"}
        r = requests.get(
            f"{SEMGREP_URL}api/agent/deployments", timeout=10, headers=headers
        )
        if r.ok:
            data = r.json()
            return data.get("deployment", {}).get("id")  # type: ignore
        else:
            return None

    @staticmethod
    def get_token() -> Optional[str]:
        """
        Get saved token in following order:
        - env var
        - settings file
        - None
        """
        login_token = os.environ.get(
            Authentication.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME, None
        )
        if login_token is not None:
            logger.debug(
                f"Using environment variable {Authentication.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME} as api token"
            )
            return login_token

        return Authentication._read_token_from_settings_file()

    @staticmethod
    def _read_token_from_settings_file() -> Optional[str]:
        """
        Read api token from settings file

        Returns None if api token not in settings file
        """
        logger.debug("Getting API token from settings file")
        login_token = SETTINGS.get_setting(
            Authentication.SEMGREP_API_TOKEN_SETTINGS_KEY, default=None
        )

        if login_token and not isinstance(login_token, str):
            raise ValueError()

        return login_token

    @staticmethod
    def set_token(token: str) -> None:
        """
        Save api token to settings file
        """
        logger.debug("Saving API token in settings file")
        SETTINGS.add_setting(Authentication.SEMGREP_API_TOKEN_SETTINGS_KEY, token)

    @staticmethod
    def delete_token() -> None:
        """
        Remove api token from settings file
        """
        logger.debug("Deleting api token from settings file")
        SETTINGS.delete_setting(Authentication.SEMGREP_API_TOKEN_SETTINGS_KEY)

    @staticmethod
    def is_a_tty() -> bool:
        """
        Whether or not the terminal is interactive (a tty)
        Separated out to make test mocking easier
        """
        return sys.stderr.isatty()
