#
# Copyright (c) 2022-2024 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import sys
from typing import Any
from typing import Dict
from typing import Optional

import requests
from attr import define
from attr import Factory
from attr import field

from semgrep.error import error_type_string
from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepCoreError
from semgrep.util import redact_credentials
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def _format_core_error(e: SemgrepCoreError) -> str:
    """
    Like traceback.format_exception, but the last line is built from the
    error's type, rule, and exit code instead of str(e): the core message can
    quote scanned source code, the details carry the core's exception and
    backtrace, and the location is a path in the customer's repository.
    """
    import traceback

    frames = "".join(traceback.format_tb(e.__traceback__))
    rule = f" (rule {e.core.rule_id.value})" if e.core.rule_id else ""
    return (
        "Traceback (most recent call last):\n"
        f"{frames}"
        f"{type(e).__name__}: {error_type_string(e.core.error_type)}{rule}, "
        f"exit code {e.code}\n"
    )


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

        # The traceback is POSTed to the fail-open endpoint; scrub before store.
        current = sys.exc_info()[1] or e
        if isinstance(current, SemgrepCoreError):
            self.payload["error"] = redact_credentials(_format_core_error(current))
        elif sys.exc_info()[0] is not None:
            self.payload["error"] = redact_credentials("".join(traceback.format_exc()))
        elif e is not None:
            exc_type = e.__class__
            tb = e.__traceback__
            self.payload["error"] = redact_credentials(
                "".join(traceback.format_exception(exc_type, e, tb))
            )

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
        self.payload["exit_code"] = exit_code

        try:
            requests.post(url, headers=headers, json=self.payload, timeout=3)
        except Exception as e:
            logger.debug(f"Error sending to fail-open endpoint: {e}")

        return 0
