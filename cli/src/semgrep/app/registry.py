import json
import logging
from typing import List

from semgrep.error import SemgrepError
from semgrep.state import get_state
from semgrep.types import JsonObject

logger = logging.getLogger(__name__)


def list_current_public_rulesets() -> List[JsonObject]:
    state = get_state()
    api_full_url = f"{state.env.semgrep_url}/api/registry/ruleset"
    try:
        r = state.app_session.get(api_full_url)
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
