import json
import logging
from typing import List

from semgrep.app import app_session
from semgrep.constants import SEMGREP_URL
from semgrep.error import SemgrepError
from semgrep.types import JsonObject

logger = logging.getLogger(__name__)


def list_current_public_rulesets() -> List[JsonObject]:
    api_full_url = f"{SEMGREP_URL}/api/registry/ruleset"
    try:
        r = app_session.get(api_full_url)
    except Exception:
        raise SemgrepError(f"Failed to download list of public rulesets")

    if not r.ok:
        raise SemgrepError(
            f"Bad status code: {r.status_code} returned by url: {api_full_url}"
        )

    logger.debug(f"Retrieved rulesets: {r.text}")
    try:
        ruleset_json = json.loads(r.text)
    except json.decoder.JSONDecodeError as e:
        raise SemgrepError(f"Failed to parse rulesets as valid json")

    return ruleset_json  # type:ignore
