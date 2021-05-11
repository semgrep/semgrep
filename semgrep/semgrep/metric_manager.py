import logging
import os
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

import requests

from semgrep.constants import SEMGREP_USER_AGENT

METRICS_ENDPOINT = "https://stats.semgrep.dev"

logger = logging.getLogger(__name__)


class _MetricManager:
    """

    Made explicit decision to be verbose in setting metrics instead
    of something more dynamic (and thus less boiler plate code) to
    be very explicit in what metrics are being collected
    """

    def __init__(self) -> None:
        self._return_code: Optional[int] = None
        self._version: Optional[str] = None
        self._num_rules: Optional[int] = None
        self._num_targets: Optional[int] = None
        self._num_findings: Optional[int] = None
        self._num_ignored: Optional[int] = None
        self._run_time: Optional[float] = None
        self._total_bytes_scanned: Optional[int] = None
        self._errors: List[str] = []

    def set_return_code(self, return_code: int) -> None:
        self._return_code = return_code

    def set_version(self, version: str) -> None:
        self._version = version

    def set_num_rules(self, num_rules: int) -> None:
        self._num_rules = num_rules

    def set_num_targets(self, num_targets: int) -> None:
        self._num_targets = num_targets

    def set_num_findings(self, num_findings: int) -> None:
        self._num_findings = num_findings

    def set_num_ignored(self, num_ignored: int) -> None:
        self._num_ignored = num_ignored

    def set_run_time(self, run_time: float) -> None:
        self._run_time = run_time

    def set_total_bytes_scanned(self, total_bytes_scanned: int) -> None:
        self._total_bytes_scanned = total_bytes_scanned

    def set_errors(self, error_types: List[str]) -> None:
        self._errors = error_types

    def as_dict(self) -> Dict[str, Any]:
        return {
            "return_code": self._return_code,
            "version": self._version,
            "num_rules": self._num_rules,
            "num_targets": self._num_targets,
            "num_findings": self._num_findings,
            "num_ignored": self._num_ignored,
            "run_time": self._run_time,
            "total_bytes_scanned": self._total_bytes_scanned,
            "errors": self._errors,
        }

    def send(self) -> None:
        """
        Send metrics to the metrics server. Is a noop if SEMGREP_SEND_METRICS
        env var is not set
        """
        if os.environ.get("SEMGREP_SEND_METRICS"):
            metrics = self.as_dict()
            headers = {"User-Agent": SEMGREP_USER_AGENT}

            try:
                r = requests.post(
                    METRICS_ENDPOINT, json=metrics, timeout=10, headers=headers
                )
                r.raise_for_status()
                logger.info("Sent anonymized metrics.")
            except Exception as e:
                logger.info("Failed to send anonymized metrics.")


metric_manager = _MetricManager()
