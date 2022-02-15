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
    # @Tin to pick the URL here
    return (
        session_id,
        f"{SEMGREP_URL}login-cli?t={session_id}&docker={IN_DOCKER}&gha={IN_GH_ACTION}",
    )


@click.command()
def login() -> None:
    """
    Prompts for API key to semgrep.dev and saves it to global settings file
    """
    saved_login_token = Authentication.read_token()
    if saved_login_token:
        click.echo(
            f"API token already exists in {SETTINGS.get_path_to_settings()}. To login with a different token logout with `semgrep logout`"
        )
        sys.exit(1)

    if sys.stderr.isatty():
        # TODO: give a way to pass --app-token or something as a flag
        # login_token = click.prompt("Enter semgrep.dev API token", hide_input=True)
        session_id, url = make_login_url()
        click.echo(
            "Login enables additional proprietary Semgrep Registry rules and running custom policies from Semgrep App."
        )
        click.echo(f"Login at: {url}")
        click.echo(
            "\nOnce you've logged in, return here and you'll be ready to start using new Semgrep rules."
        )
        MAX_RETRIES = 10
        WAIT_BETWEEN_RETRY_IN_SEC = 5
        import requests

        for _ in range(MAX_RETRIES):
            headers = {"User-Agent": SEMGREP_USER_AGENT}
            r = requests.get(
                # @Tin not sure what this URL should be
                f"{SEMGREP_URL}api/agent/deployment",
                timeout=10,
                headers=headers,
            )
            if r.status_code == 200:
                as_json = r.json()
                if not save_token(as_json.get("token")):
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
            "Error: semgrep login is an interactive command: must run in an interactive terminal",
            err=True,
        )
        sys.exit(1)


def save_token(login_token: Optional[str]) -> bool:
    if login_token is not None and Authentication.is_valid_token(login_token):
        Authentication.set_token(login_token)
        click.echo(
            f"Saved login token\n\n\t{login_token}\n\n in {SETTINGS.get_path_to_settings()}."
        )
        click.echo(f"Note: You can always generate more tokens at {SEMGREP_URL}")
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
    click.echo("logged out")


class Authentication:
    SEMGREP_LOGIN_TOKEN_ENVVAR_NAME = "SEMGREP_LOGIN_TOKEN"
    SEMGREP_API_TOKEN_SETTINGS_KEY = "api_token"

    @staticmethod
    def is_valid_token(token: str) -> bool:
        """
        Returns true if token is valid
        """
        import requests

        headers = {"User-Agent": SEMGREP_USER_AGENT, "Authorization": f"Bearer {token}"}
        r = requests.get(
            f"{SEMGREP_URL}api/agent/deployment", timeout=10, headers=headers
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
            f"{SEMGREP_URL}api/agent/deployment", timeout=10, headers=headers
        )
        data = r.json()
        return data.get("deployment", {}).get("id")  # type: ignore

    @staticmethod
    def get_token() -> Optional[str]:
        """
        Get saved token in following order:
        - env var
        - settings file
        - None
        """
        login_token = os.environ.get(Authentication.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME)
        if login_token:
            logger.debug(
                f"Using environment variable {Authentication.SEMGREP_LOGIN_TOKEN_ENVVAR_NAME} as api token"
            )
            return login_token

        return Authentication.read_token()

    @staticmethod
    def read_token() -> Optional[str]:
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
