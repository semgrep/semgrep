import hashlib
import logging
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.rule import Rule


METRICS_ENDPOINT = "https://metrics.semgrep.dev"

logger = logging.getLogger(__name__)


class _MetricManager:
    """
    To prevent sending unintended metrics, be sure that any data
    stored on this object is sanitized of anything that we don't
    want sent (i.e. sanitize before saving not before sending)

    Made explicit decision to be verbose in setting metrics instead
    of something more dynamic (and thus less boiler plate code) to
    be very explicit in what metrics are being collected
    """

    def __init__(self) -> None:
        self._project_hash: Optional[str] = None
        self._configs_hash = ""
        self._rules_hash = ""
        self._return_code: Optional[int] = None
        self._version: Optional[str] = None
        self._num_rules: Optional[int] = None
        self._num_targets: Optional[int] = None
        self._num_findings: Optional[int] = None
        self._num_ignored: Optional[int] = None
        self._run_time: Optional[float] = None
        self._total_bytes_scanned: Optional[int] = None
        self._errors: List[str] = []

        self._send_metrics = False

    def set_project_hash(self, project_hash: Optional[str]) -> None:
        self._project_hash = project_hash

    def set_configs_hash(self, configs: List[str]) -> None:
        """
        Assumes configs is list of arguments passed to semgrep using --config
        """
        m = hashlib.sha256()
        for c in configs:
            m.update(c.encode())
        self._configs_hash = m.hexdigest()

    def set_rules_hash(self, rules: List[Rule]) -> None:
        m = hashlib.sha256()
        for r in rules:
            m.update(r.full_hash.encode())
        self._rules_hash = m.hexdigest()

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
            "environment": {
                "version": self._version,
                "projectHash": self._project_hash,
                "configNamesHash": self._configs_hash,
                "rulesHash": self._rules_hash,
            },
            "performance": {
                "runTime": self._run_time,
                "numRules": self._num_rules,
                "numTargets": self._num_targets,
                "totalBytesScanned": self._total_bytes_scanned,
            },
            "errors": {
                "returnCode": self._return_code,
                "errors": self._errors,
            },
            "value": {
                "numFindings": self._num_findings,
                "numIgnored": self._num_ignored,
            },
        }

    def disable(self) -> None:
        self._send_metrics = False

    def enable(self) -> None:
        self._send_metrics = True

    def send(self) -> None:
        """
        Send metrics to the metrics server.

        Will if _send_metrics is True
        """
        import requests

        if self._send_metrics:
            metrics = self.as_dict()
            headers = {"User-Agent": SEMGREP_USER_AGENT}

            try:
                r = requests.post(
                    METRICS_ENDPOINT, json=metrics, timeout=2, headers=headers
                )
                r.raise_for_status()
                logger.debug("Sent non-identifiable metrics")
            except Exception as e:
                logger.debug(f"Failed to send non-identifiable metrics: {e}")


metric_manager = _MetricManager()
