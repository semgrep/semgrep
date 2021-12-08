import hashlib
import os
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from urllib.parse import urlparse

import click

from semgrep.constants import SEMGREP_USER_AGENT
from semgrep.profiling import ProfilingData
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.types import MetricsState
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

METRICS_ENDPOINT = "https://metrics.semgrep.dev"


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
        self._profiling_times: Dict[str, float] = {}
        self._total_bytes_scanned: Optional[int] = None
        self._errors: List[str] = []
        self._file_stats: List[Dict[str, Any]] = []
        self._rule_stats: List[Dict[str, Any]] = []
        self._rules_with_findings: Mapping[str, int] = {}

        self._send_metrics: MetricsState = MetricsState.OFF
        self._using_server = False

    def configure(
        self,
        metrics_state: Optional[MetricsState],
        legacy_state: Optional[MetricsState],
    ) -> None:
        """
        Configures whether to always, never, or automatically send metrics (based on whether config
        is pulled from the server).

        :param metrics_state: The value of the --metrics option
        :param legacy_state: Value of the --enable-metrics/--disable-metrics option
        :raises click.BadParameter: if both --metrics and --enable-metrics/--disable-metrics are passed
        """

        if metrics_state is not None and legacy_state is not None:
            raise click.BadParameter(
                "--enable-metrics/--disable-metrics can not be used with either --metrics or SEMGREP_SEND_METRICS"
            )
        self._send_metrics = metrics_state or legacy_state or MetricsState.AUTO

    def set_using_server_true(self) -> None:
        if not self._using_server:
            logger.info("Fetching rules from https://semgrep.dev/registry ...")

        self._using_server = True

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

    def set_configs_hash(self, configs: Sequence[str]) -> None:
        """
        Assumes configs is list of arguments passed to semgrep using --config
        """
        m = hashlib.sha256()
        for c in configs:
            m.update(c.encode())
        self._configs_hash = m.hexdigest()

    def set_rules_hash(self, rules: Sequence[Rule]) -> None:
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

    def set_profiling_times(self, profiling_times: Dict[str, float]) -> None:
        self._profiling_times = profiling_times

    def set_total_bytes_scanned(self, total_bytes_scanned: int) -> None:
        self._total_bytes_scanned = total_bytes_scanned

    def set_errors(self, error_types: List[str]) -> None:
        self._errors = error_types

    def set_rules_with_findings(
        self, findings: Mapping[Rule, Sequence[RuleMatch]]
    ) -> None:
        self._rules_with_findings = {r.full_hash: len(f) for r, f in findings.items()}

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
                "ci": os.environ.get("CI"),
            },
            "performance": {
                "fileStats": self._file_stats,
                "ruleStats": self._rule_stats,
                "profilingTimes": self._profiling_times,
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
                "ruleHashesWithFindings": self._rules_with_findings,
            },
        }

    def is_enabled(self) -> bool:
        """
        Returns whether metrics should be sent.

        If _send_metrics is:
          - auto, sends if using_server
          - on, sends
          - off, doesn't send
        """
        res = (
            self._using_server
            if self._send_metrics == MetricsState.AUTO
            else self._send_metrics == MetricsState.ON
        )
        return res

    def send(self) -> None:
        """
        Send metrics to the metrics server.

        Will if is_enabled is True
        """
        import requests

        logger.verbose(
            f"{'Sending' if self.is_enabled() else 'Not sending'} pseudonymous metrics since metrics are configured to {self._send_metrics.name} and server usage is {self._using_server}"
        )

        if self.is_enabled():
            metrics = self.as_dict()
            headers = {"User-Agent": SEMGREP_USER_AGENT}

            try:
                r = requests.post(
                    METRICS_ENDPOINT, json=metrics, timeout=2, headers=headers
                )
                r.raise_for_status()
                logger.debug("Sent pseudonymous metrics")
            except Exception as e:
                logger.debug(f"Failed to send pseudonymous metrics: {e}")


metric_manager = _MetricManager()
