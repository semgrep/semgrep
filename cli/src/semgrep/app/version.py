"""Ask the Semgrep App server about the latest Semgrep version

This module is for pinging the app to ask for the latest Semgrep release
so we can print a message prompting the user to upgrade if they have
an outdated version.
"""
# TODO: for predictable test output, add a flag to avoid making actual
# network calls?
import json
import re
import time
from dataclasses import dataclass
from json import JSONDecodeError
from pathlib import Path
from typing import cast
from typing import Dict
from typing import List
from typing import Literal
from typing import Mapping
from typing import Optional
from typing import Set

import requests
from packaging.version import InvalidVersion
from packaging.version import Version

from semgrep import __VERSION__
from semgrep.constants import Colors
from semgrep.state import get_state
from semgrep.types import JsonObject
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

# NOTE: Subscriptable types require Python >= 3.9
# and we still support Python 3.8 :(

# Typings for keys of valid identifiers for the banners
ValidIdentifers = Literal["upgrade", "too_many_findings"]

# Identifiers that should be treated as conditional and skipped during the base banner display
CONDITIONAL_IDENTIFIERS: Set[ValidIdentifers] = {"too_many_findings"}

# Mapping of valid identifiers to their corresponding emoji icon
IDENTIFIER_LOOKUP: Dict[ValidIdentifers, str] = {
    "upgrade": "⏫",
    "too_many_findings": "📢",  # loud speaker icon
}
TOO_MANY_FINDINGS_THRESHOLD = 25


# NOTE: We should expect the API response to be in the following format.
# We do not define this here as the source of truth is in the proto definition
# in the Semgrep App repository, and this is used just as a reference.

# @dataclass
# class Banner:
#     message: str
#     show_version: Optional[str]
#     hide_version: Optional[str]
#     url: Optional[str]
#     identifier: Optional[str]

# @dataclass
# class VersionApiResponse:
#     latest: str
#     versions: Dict[Literal["latest", "minimum"], str]
#     banners: List[Banner]
#     no_findings_msg: str


@dataclass
class VersionInfo:
    current: Version
    api_response: JsonObject


def _fetch_latest_version() -> Optional[JsonObject]:
    state = get_state()

    try:
        resp = state.app_session.get(
            state.env.version_check_url, timeout=state.env.version_check_timeout
        )
    except Exception as e:
        logger.debug(f"Fetching latest version failed to connect: {e}")
        return None

    if resp.status_code != requests.codes.OK:
        logger.debug(
            f"Fetching latest version received HTTP error code: {resp.status_code}"
        )
        return None
    try:
        res = resp.json()
    except ValueError:
        logger.debug("Fetching latest version received invalid JSON")
        return None

    if not isinstance(res, Mapping):
        logger.debug("Latest version response is not an object")
        return None

    return res


def _get_version_from_cache(version_cache_path: Path) -> Optional[JsonObject]:
    now = time.time()

    if not version_cache_path.is_file():
        logger.debug("Version cache does not exist")
        return None

    with version_cache_path.open() as f:
        timestamp_str = f.readline().strip()
        latest_version_str = f.readline().strip()

    try:
        # Treat time as integer seconds so no need to deal with str float conversion
        timestamp = int(timestamp_str)
    except ValueError:
        logger.debug(f"Version cache invalid timestamp: {timestamp_str}")
        return None

    one_day = 86400
    if now - timestamp > one_day:
        logger.debug(f"Version cache expired: {timestamp_str}:{now}")
        return None

    try:
        res = json.loads(latest_version_str)
    except JSONDecodeError:
        logger.debug(f"Version cache does not contain JSON object")
        return None

    if not isinstance(res, Mapping):
        logger.debug("Latest version response is not an object")
        return None

    return res


def _get_latest_version(allow_fetch: bool = True) -> Optional[JsonObject]:
    env = get_state().env
    latest_version = _get_version_from_cache(env.version_check_cache_path)

    if latest_version is None and allow_fetch:
        latest_version = _fetch_latest_version()

    if latest_version is None:
        # Request timed out or invalid
        return None

    env.version_check_cache_path.parent.mkdir(parents=True, exist_ok=True)
    with env.version_check_cache_path.open("w") as f:
        # Integer time so no need to deal with str float conversions
        f.write(f"{int(time.time())}\n")
        f.write(json.dumps(latest_version))

    return latest_version


def _get_version_info() -> Optional[VersionInfo]:
    latest_version_object = _get_latest_version()
    if latest_version_object is None:
        return None
    try:
        current_version = Version(__VERSION__)
    except InvalidVersion as e:
        logger.debug(f"Invalid version string: {e}")
        return None
    return VersionInfo(current_version, latest_version_object)


def _get_version_filtered_banners() -> List[JsonObject]:
    """
    Filters banners based on the current version of the CLI and version ranges specified by the API response.
    """
    filtered_banners: List[JsonObject] = []
    latest_version_info = _get_version_info()
    if not latest_version_info:
        return filtered_banners

    api_response = latest_version_info.api_response
    current_version = latest_version_info.current
    banners = api_response.get("banners", [])

    for b in banners:
        try:
            show_str = b.get("show_version")  # Note that b["show_version"] can be None
            show = Version(show_str) if show_str else None
            hide_str = b.get("hide_version")
            hide = Version(hide_str) if hide_str else None
        except InvalidVersion as e:
            logger.debug(f"Invalid version string: {e}")
            continue
        if (not show or current_version >= show) and (
            not hide or current_version < hide
        ):
            filtered_banners.append(b)
    return filtered_banners


def _show_banners() -> None:
    logged_something = False
    banners = _get_version_filtered_banners()
    state = get_state()
    for b in banners:
        try:
            message = b.get("message", "")
            identifier = cast(ValidIdentifers, b.get("identifier", "") or "")
            icon = IDENTIFIER_LOOKUP.get(identifier) or "⏫"
            if not message:
                continue
            if identifier in CONDITIONAL_IDENTIFIERS:
                # If we know on the CLI-side that banner is marked as conditional,
                # we will need to perform additional checks to determine if we should show the banner.
                continue
            if state.env.with_new_cli_ux:
                logger.warning(f"\n{icon} {message}")
            else:
                logger.warning(f"\n{message}")
            logged_something = True
        except Exception as e:
            logger.debug(f"Error processing banner: {e}")

    env = get_state().env
    if logged_something and env.in_agent:
        logger.warning(
            "If you're using the returntocorp/semgrep-agent:v1 image, you will be automatically upgraded within 24 hours."
        )


def version_check() -> None:
    """
    Checks for messages from the backend, displaying any messages that match the current version
    """
    _show_banners()


def get_no_findings_msg() -> Optional[str]:
    """
    Gets and returns the latest no_findings message from the backend from cache.

    Will only ever return a response if version_check finished before this call.
    """
    # only the real version_check request should be allowed to send a request to semgrep.dev
    # so that we can gate only the version checks behind `not --disable-version-check` conditions
    latest_version_object = _get_latest_version(allow_fetch=False)
    if latest_version_object is None or "no_findings_msg" not in latest_version_object:
        return None
    state = get_state()
    base_msg = str(latest_version_object["no_findings_msg"])
    if not state.env.with_new_cli_ux:
        return base_msg
    msg = re.sub("\n(\n+)?", "\\1\n   ", base_msg)
    groups = re.split(r"\s+(?=https:)", msg, 1)
    if len(groups) == 2:
        pretty_url = with_color(Colors.cyan, f"{groups[1]}", underline=True)
        return f"\n✨ {groups[0]} {pretty_url}"
    return f"\n✨ {msg}"


def get_too_many_findings_msg() -> Optional[str]:
    """
    Returns the latest too_many_findings message from the backend or cache.

    Note that this will only return a response if the version_check operation was completed before this call,
    or if we have a locally cached response.
    """
    banners = _get_version_filtered_banners()
    too_many_findings_banner = next(
        (b for b in banners if (b.get("identifier") or "") == "too_many_findings"), None
    )
    if not too_many_findings_banner:
        return None
    message = too_many_findings_banner.get("message") or ""
    identifier = cast(
        ValidIdentifers, too_many_findings_banner.get("identifier", "") or ""
    )
    icon = IDENTIFIER_LOOKUP.get(identifier) or "⏫"
    url = too_many_findings_banner.get("url") or ""
    pretty_url = with_color(Colors.cyan, f"{url}", underline=True) if url else ""
    suffix = f"\n   See {pretty_url}." if pretty_url else ""
    if not message:
        return None
    return f"\n{icon} {message}{suffix}"
