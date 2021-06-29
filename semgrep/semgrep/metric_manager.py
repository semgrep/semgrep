import hashlib
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from urllib.parse import urlparse

from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.profiling import ProfilingData
from semgrep.rule import Rule
from semgrep.verbose_logging import getLogger

METRICS_ENDPOINT = "https://metrics.semgrep.dev"

logger = getLogger(__name__)


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
        self._file_stats: List[Dict[str, Any]] = []
        self._rule_stats: List[Dict[str, Any]] = []

        self._send_metrics = False

    def set_project_hash(self, project_url: Optional[str]) -> None:
        """
        Standardizes url then hashes
        """
        if project_url is None:
            self._project_hash = None
        else:
            try:
                parsed_url = urlparse(project_url)
                if parsed_url.scheme == "https":
                    # Remove optional username/password from project_url
                    sanitized_url = f"{parsed_url.hostname}{parsed_url.path}"
                else:
                    # For now don't do anything special with other git-url formats
                    sanitized_url = project_url
            except ValueError as e:
                logger.debug(f"Failed to parse url {project_url}")
                sanitized_url = project_url

            project_hash = hashlib.sha256(sanitized_url.encode()).hexdigest()
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
        rule_hashes = [r.full_hash for r in rules]
        rule_hashes.sort()  # sort hashes to have a stable rules_hash
        for rule_hash in rule_hashes:
            m.update(rule_hash.encode())
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

    def set_run_timings(
        self, profiling_data: ProfilingData, targets: List[Path], rules: List[Rule]
    ) -> None:
        """
        Store rule hashes, rule parse times, and file-stats
        """
        rule_stats = []
        for rule in rules:
            rule_stats.append(
                {
                    "ruleHash": rule.full_hash,
                    "parseTime": profiling_data.get_rule_parse_time(rule),
                    "matchTime": profiling_data.get_rule_match_time(rule),
                    "runTime": profiling_data.get_rule_run_time(rule),
                    "bytesScanned": profiling_data.get_rule_bytes_scanned(rule),
                }
            )
        self._rule_stats = rule_stats

        file_stats = []
        for target in targets:
            file_stats.append(
                {
                    "size": target.stat().st_size,
                    "numTimesScanned": profiling_data.get_file_num_times_scanned(
                        target
                    ),
                    "parseTime": profiling_data.get_file_parse_time(target),
                    "matchTime": profiling_data.get_file_match_time(target),
                    "runTime": profiling_data.get_file_run_time(target),
                }
            )
        self._file_stats = file_stats

    def as_dict(self) -> Dict[str, Any]:
        return {
            "environment": {
                "version": self._version,
                "projectHash": self._project_hash,
                "configNamesHash": self._configs_hash,
                "rulesHash": self._rules_hash,
            },
            "performance": {
                "fileStats": self._file_stats,
                "ruleStats": self._rule_stats,
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

    @property
    def is_enabled(self) -> bool:
        return self._send_metrics

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
