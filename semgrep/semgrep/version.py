import json
import os
import time
from json import JSONDecodeError
from pathlib import Path
from typing import Mapping
from typing import Optional

from packaging.version import InvalidVersion
from packaging.version import Version

from semgrep import __VERSION__
from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.types import JsonObject
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

VERSION_CHECK_URL = str(
    os.environ.get("SEMGREP_VERSION_CHECK_URL", "https://semgrep.dev/api/check-version")
)
VERSION_CHECK_TIMEOUT = int(
    os.environ.get(
        "SEMGREP_VERSION_CHECK_TIMEOUT", 2  # Don't block user's for too long
    )
)
VERSION_CACHE_PATH = Path(
    os.environ.get(
        "SEMGREP_VERSION_CACHE_PATH",
        Path.home() / ".cache" / "semgrep_version",
    )
)


def _fetch_latest_version(
    url: str = VERSION_CHECK_URL, timeout: int = VERSION_CHECK_TIMEOUT
) -> Optional[JsonObject]:
    try:
        import requests

        resp = requests.get(
            url,
            headers={"User-Agent": SEMGREP_USER_AGENT},
            timeout=timeout,
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


def _get_latest_version(version_cache_path: Path) -> Optional[JsonObject]:
    latest_version = _get_version_from_cache(version_cache_path)

    if latest_version is None:
        latest_version = _fetch_latest_version()

    if latest_version is None:
        # Request timed out or invalid
        return None

    version_cache_path.parent.mkdir(parents=True, exist_ok=True)
    with version_cache_path.open("w") as f:
        # Integer time so no need to deal with str float conversions
        f.write(f"{int(time.time())}\n")
        f.write(json.dumps(latest_version))

    return latest_version


def _show_banners(current_version: Version, latest_version_object: JsonObject) -> None:
    banners = latest_version_object.get("banners", [])
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
            logger.warning("\n" + b.get("message", ""))


def version_check(version_cache_path: Path = VERSION_CACHE_PATH) -> None:
    """
    Checks for messages from the backend, displaying any messages that match the current version

    :param version_cache_path: Path where we cache the backend response
    """
    latest_version_object = _get_latest_version(version_cache_path)
    if latest_version_object is None:
        return

    try:
        current_version = Version(__VERSION__)
    except InvalidVersion as e:
        logger.debug(f"Invalid version string: {e}")
        return

    _show_banners(current_version, latest_version_object)
