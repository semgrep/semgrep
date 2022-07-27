import sys
from typing import Any
from typing import Dict
from typing import Optional

import requests
from attr import define
from attr import Factory
from attr import field

from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@define
class ErrorHandler:
    """
    Send scan status to the fail-open endpoint
    """

    suppress_errors: bool = field(default=False)
    payload: Dict[str, Any] = Factory(dict)

    def configure(self, suppress_errors: Optional[bool] = False) -> None:
        """
        Configures whether to always or never send fail-open status.

        :param suppress_errors: The value of the --suppress-errors option
        """
        if suppress_errors:
            self.suppress_errors = suppress_errors

    def push_request(self, method: str, url: str, **kwargs: Any) -> None:
        self.payload = {"method": method, "url": url, **kwargs}

    def append_request(self, **kwargs: Any) -> None:
        self.payload = {**self.payload, **kwargs}

    def pop_request(self) -> None:
        self.payload = {}

    @property
    def is_enabled(self) -> bool:
        """
        Returns whether scan status should be sent.
        """
        return self.suppress_errors

    def send(self, exit_code: int) -> int:
        """
        Send scan status to the fail-open server.

        Status will only be emitted if is_enabled is True
        """
        from semgrep.state import get_state  # avoiding circular import

        state = get_state()

        if not self.is_enabled or exit_code == 0:
            return exit_code

        import traceback

        logger.debug(
            f"Sending to fail-open endpoint {state.env.fail_open_url} since fail-open is configured to {self.suppress_errors}"
        )

        headers = self.payload.get("headers", {})
        headers["User-Agent"] = str(state.app_session.user_agent)
        self.payload["headers"] = headers
        if sys.exc_info()[0] is not None:
            self.payload["error"] = traceback.format_exc()
        try:
            requests.post(state.env.fail_open_url, json=self.payload, timeout=3)
        except Exception as e:
            logger.error(f"Error sending to fail-open endpoint: {e}")
        return 0
