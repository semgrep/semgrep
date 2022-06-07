import logging
import sys
from typing import Optional

from semgrep.state import get_state

logger = logging.getLogger(__name__)


def is_valid_token(token: str) -> bool:
    """
    Returns true if token is valid
    """
    state = get_state()
    r = state.app_session.get(
        f"{state.env.semgrep_url}/api/agent/deployments/current",
        headers={"Authorization": f"Bearer {token}"},
    )
    return r.ok


def get_deployment_id() -> Optional[int]:
    """
    Returns the deployment_id attached to an api_token as int

    Returns None if api_token is invalid/doesn't have associated deployment
    """
    state = get_state()
    r = state.app_session.get(f"{state.env.semgrep_url}/api/agent/deployments/current")

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
    state = get_state()
    if state.env.app_token is not None:
        logger.debug(f"Using environment variable SEMGREP_APP_TOKEN as api token")
        return state.env.app_token

    return _read_token_from_settings_file()


def _read_token_from_settings_file() -> Optional[str]:
    """
    Read api token from settings file

    Returns None if api token not in settings file
    """
    logger.debug("Getting API token from settings file")
    settings = get_state().settings
    login_token = settings.get("api_token")

    if login_token and not isinstance(login_token, str):
        raise ValueError()

    return login_token


def set_token(token: str) -> None:
    """
    Save api token to settings file
    """
    logger.debug("Saving API token in settings file")
    settings = get_state().settings
    settings.set("api_token", token)


def delete_token() -> None:
    """
    Remove api token from settings file
    """
    logger.debug("Deleting api token from settings file")
    settings = get_state().settings
    settings.delete("api_token")


def is_a_tty() -> bool:
    """
    Whether or not the terminal is interactive (a tty)
    Separated out to make test mocking easier
    """
    return sys.stderr.isatty()
