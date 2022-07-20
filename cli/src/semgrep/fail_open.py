from enum import auto
from enum import Enum
from typing import Any
from typing import Dict
from typing import Optional

import requests
from attr import define
from attr import Factory

from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class FailOpenState(Enum):
    """
    Configures metrics upload.

    ON - Fail-open always sent
    OFF - Fail-open never sent
    """

    ON = auto()
    OFF = auto()


@define
class FailOpen:
    """
    Send scan status to the fail-open endpoint
    """

    fail_open_state: FailOpenState = FailOpenState.OFF
    payload: Dict[str, Any] = Factory(dict)

    def configure(self, fail_open_state: Optional[FailOpenState]) -> None:
        """
        Configures whether to always or never send fail-open status.

        :param fail_open_state: The value of the --fail-open option
        """
        if fail_open_state:
            self.fail_open_state = fail_open_state

    def push_request(self, method: str, url: str, **kwargs: Any) -> None:
        self.payload = {"method": method, "url": url, **kwargs}

    def pop_request(self) -> None:
        self.payload = {}

    @property
    def is_enabled(self) -> bool:
        """
        Returns whether scan status should be sent.

        If fail_open_state is:
          - on, sends
          - off, doesn't send
        """
        return self.fail_open_state == FailOpenState.ON

    def send(self, exit_code: int) -> int:
        """
        Send scan status to the fail-open server.

        Will if is_enabled is True
        """
        from semgrep.state import get_state  # avoiding circular import

        state = get_state()

        if not self.is_enabled or exit_code == 0:
            return exit_code

        import traceback

        logger.debug(
            f"Sending to fail-open endpoint {state.env.fail_open_url} since fail-open is configured to {self.fail_open_state.name}"
        )

        headers = self.payload.get("headers", {})
        headers["User-Agent"] = str(state.app_session.user_agent)
        self.payload["headers"] = headers
        self.payload["error"] = traceback.format_exc()
        requests.post(state.env.fail_open_url, json=self.payload, timeout=3)
        return 0
