import sys
from typing import Any
from typing import Dict
from typing import Optional

import requests
from attr import define
from attr import Factory
from attr import field

from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import OK_EXIT_CODE
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
        self.payload = {"method": method, "url": url}

    def append_request(self, **kwargs: Any) -> None:
        self.payload = {**self.payload, **kwargs}

    def pop_request(self) -> None:
        self.payload = {}

    def capture_error(self, e: Optional[BaseException] = None) -> None:
        import traceback

        if sys.exc_info()[0] is not None:
            self.payload["error"] = "".join(traceback.format_exc())
        elif e is not None:
            exc_type = e.__class__
            tb = e.__traceback__
            self.payload["error"] = "".join(traceback.format_exception(exc_type, e, tb))

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
        from semgrep.app import auth  # avoiding circular import

        state = get_state()

        if (
            not self.is_enabled
            or exit_code == OK_EXIT_CODE
            or exit_code == FINDINGS_EXIT_CODE
        ):
            return exit_code

        url = f"{state.env.fail_open_url}/failure"

        logger.error(
            "There were errors during analysis but Semgrep will succeed because there were no blocking findings, use --no-suppress-errors if you want Semgrep to fail when there are errors."
        )

        logger.debug(
            f"Sending to fail-open endpoint {url} since fail-open is configured to {self.suppress_errors}"
        )

        token = auth.get_token()
        headers = {
            "User-Agent": str(state.app_session.user_agent),
            "Authorization": f"Bearer {token or ''}",
        }
        if "error" not in self.payload:
            self.capture_error()

        self.payload["request_id"] = str(state.local_scan_id)

        try:
            requests.post(url, headers=headers, json=self.payload, timeout=3)
        except Exception as e:
            logger.debug(f"Error sending to fail-open endpoint: {e}")

        return 0
