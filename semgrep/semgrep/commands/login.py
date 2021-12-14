import os
import sys
from typing import Optional

import click

from semgrep.constants import SEMGREP_URL
from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.settings import SETTINGS
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


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

    login_token = click.prompt("Enter semgrep.dev API token", hide_input=True)
    if Authentication.is_valid_token(login_token):
        Authentication.set_token(login_token)
        click.echo(f"Valid API Token saved in {SETTINGS.get_path_to_settings()}")
    else:
        click.echo("entered token is not valid. Please try again.")
        sys.exit(1)


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
            f"{SEMGREP_URL}/api/agent/deployment", timeout=10, headers=headers
        )
        return r.ok

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
