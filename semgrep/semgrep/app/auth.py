import logging
import os
import sys
from typing import Optional

from semgrep.constants import SEMGREP_URL
from semgrep.settings import SETTINGS

logger = logging.getLogger(__name__)

SEMGREP_LOGIN_TOKEN_ENVVAR_NAME = "SEMGREP_APP_TOKEN"
SEMGREP_API_TOKEN_SETTINGS_KEY = "api_token"


def is_valid_token(token: str) -> bool:
    """
    Returns true if token is valid
    """
    from semgrep.app import app_session  # avoiding circular imports

    r = app_session.get(
        f"{SEMGREP_URL}/api/agent/deployments/current",
        headers={"Authorization": f"Bearer {token}"},
    )
    return r.ok


def get_deployment_id() -> Optional[int]:
    """
    Returns the deployment_id attached to an api_token as int

    Returns None if api_token is invalid/doesn't have associated deployment
    """
    from semgrep.app import app_session  # avoiding circular imports

    r = app_session.get(f"{SEMGREP_URL}/api/agent/deployments/current")

    if r.ok:
        data = r.json()
        return data.get("deployment", {}).get("id")  # type: ignore
    else:
        return None


def get_token() -> Optional[str]:
    """
    Get saved token in following order:
    - env var
    - settings file
    - None
    """
    login_token = os.environ.get(SEMGREP_LOGIN_TOKEN_ENVVAR_NAME)
    if login_token is not None:
        logger.debug(
            f"Using environment variable {SEMGREP_LOGIN_TOKEN_ENVVAR_NAME} as api token"
        )
        return login_token

    return _read_token_from_settings_file()


def _read_token_from_settings_file() -> Optional[str]:
    """
    Read api token from settings file

    Returns None if api token not in settings file
    """
    logger.debug("Getting API token from settings file")
    login_token = SETTINGS.get_setting(SEMGREP_API_TOKEN_SETTINGS_KEY, default=None)

    if login_token and not isinstance(login_token, str):
        raise ValueError()

    return login_token


def set_token(token: str) -> None:
    """
    Save api token to settings file
    """
    logger.debug("Saving API token in settings file")
    SETTINGS.add_setting(SEMGREP_API_TOKEN_SETTINGS_KEY, token)


def delete_token() -> None:
    """
    Remove api token from settings file
    """
    logger.debug("Deleting api token from settings file")
    SETTINGS.delete_setting(SEMGREP_API_TOKEN_SETTINGS_KEY)


def is_a_tty() -> bool:
    """
    Whether or not the terminal is interactive (a tty)
    Separated out to make test mocking easier
    """
    return sys.stderr.isatty()
