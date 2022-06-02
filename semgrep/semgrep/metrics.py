import hashlib
import json
import os
import uuid
from datetime import datetime
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import List
from typing import NewType
from typing import Optional
from typing import Sequence
from typing import Set
from urllib.parse import urlparse

import click
import requests
from attr import define
from attr import Factory
from typing_extensions import TypedDict

from semgrep import __VERSION__
from semgrep.error import SemgrepError
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.rule import Rule
from semgrep.types import FilteredMatches
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

METRICS_ENDPOINT = "https://metrics.semgrep.dev"


class MetricsState(Enum):
    """
    Configures metrics upload.

    ON - Metrics always sent
    OFF - Metrics never sent
    AUTO - Metrics only sent if config is pulled from the server
    """

    ON = auto()
    OFF = auto()
    AUTO = auto()


Sha256Hash = NewType("Sha256Hash", str)


class RuleStats(TypedDict, total=False):
    ruleHash: str
    bytesScanned: int
    matchTime: Optional[float]


class FileStats(TypedDict, total=False):
    size: int
    numTimesScanned: int
    parseTime: Optional[float]
    matchTime: Optional[float]
    runTime: Optional[float]


class EnvironmentSchema(TypedDict, total=False):
    version: str
    projectHash: Optional[Sha256Hash]
    configNamesHash: Sha256Hash
    rulesHash: Sha256Hash
    ci: Optional[str]
    isAuthenticated: bool


class PerformanceSchema(TypedDict, total=False):
    fileStats: List[FileStats]
    ruleStats: List[RuleStats]
    profilingTimes: Dict[str, float]
    numRules: Optional[int]
    numTargets: Optional[int]
    totalBytesScanned: Optional[int]


class ErrorsSchema(TypedDict, total=False):
    returnCode: Optional[int]
    errors: List[str]


class ValueSchema(TypedDict, total=False):
    numFindings: int
    numIgnored: int
    ruleHashesWithFindings: Dict[str, int]


class TopLevelSchema(TypedDict, total=False):
    event_id: uuid.UUID
    anonymous_user_id: str
    started_at: datetime
    sent_at: datetime


class PayloadSchema(TopLevelSchema):
    environment: EnvironmentSchema
    performance: PerformanceSchema
    errors: ErrorsSchema
    value: ValueSchema


class MetricsJsonEncoder(json.JSONEncoder):
    def default(self, obj: Any) -> Any:
        if isinstance(obj, datetime):
            return obj.astimezone().isoformat()

        if isinstance(obj, uuid.UUID):
            return str(obj)

        return super().default(obj)


@define
class Metrics:
    """
    To prevent sending unintended metrics:
    1. send all data into this class with add_* methods
    2. ensure all add_* methods only set sanitized data

    These methods go directly from raw data to transported data,
    thereby skipping a "stored data" step,
    and enforcing that we sanitize before saving, not before sending.
    """

    _is_using_registry: bool = False
    metrics_state: MetricsState = MetricsState.OFF
    payload: PayloadSchema = Factory(
        lambda: PayloadSchema(
            environment=EnvironmentSchema(),
            errors=ErrorsSchema(),
            performance=PerformanceSchema(),
            value=ValueSchema(),
        )
    )

    def __attrs_post_init__(self) -> None:
        self.payload["started_at"] = datetime.now()
        self.payload["environment"]["version"] = __VERSION__
        self.payload["environment"]["ci"] = os.getenv("CI")
        self.payload["event_id"] = uuid.uuid4()

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
        and their values are different
        """

        if (
            metrics_state is not None
            and legacy_state is not None
            and metrics_state != legacy_state
        ):
            raise click.BadParameter(
                "--enable-metrics/--disable-metrics can not be used with either --metrics or SEMGREP_SEND_METRICS"
            )
        self.metrics_state = metrics_state or legacy_state or MetricsState.AUTO

    @property
    def is_using_registry(self) -> bool:
        return self._is_using_registry

    @is_using_registry.setter
    def is_using_registry(self, value: bool) -> None:
        if self.is_using_registry is False and value is True:
            logger.info("Fetching rules from https://semgrep.dev/registry.")

        self._is_using_registry = value

    def add_project_url(self, project_url: Optional[str]) -> None:
        """
        Standardizes url then hashes
        """
        if project_url is None:
            self.payload["environment"]["projectHash"] = None
            return

        try:
            parsed_url = urlparse(project_url)
            if parsed_url.scheme == "https":
                # Remove optional username/password from project_url
                sanitized_url = f"{parsed_url.hostname}{parsed_url.path}"
            else:
                # For now don't do anything special with other git-url formats
                sanitized_url = project_url
        except ValueError:
            logger.debug(f"Failed to parse url {project_url}")
            sanitized_url = project_url

        m = hashlib.sha256(sanitized_url.encode())
        self.payload["environment"]["projectHash"] = cast(Sha256Hash, m.hexdigest())

    def add_configs(self, configs: Sequence[str]) -> None:
        """
        Assumes configs is list of arguments passed to semgrep using --config
        """
        m = hashlib.sha256()
        for c in configs:
            m.update(c.encode())
        self.payload["environment"]["configNamesHash"] = cast(Sha256Hash, m.hexdigest())

    def add_rules(self, rules: Sequence[Rule], profiling_data: ProfilingData) -> None:
        rules = sorted(rules, key=lambda r: r.full_hash)
        m = hashlib.sha256()
        for rule in rules:
            m.update(rule.full_hash.encode())
        self.payload["environment"]["rulesHash"] = cast(Sha256Hash, m.hexdigest())

        self.payload["performance"]["numRules"] = len(rules)
        self.payload["performance"]["ruleStats"] = [
            {
                "ruleHash": rule.full_hash,
                "matchTime": profiling_data.get_rule_match_time(rule),
                "bytesScanned": profiling_data.get_rule_bytes_scanned(rule),
            }
            for rule in rules
        ]

    def add_findings(self, findings: FilteredMatches) -> None:
        self.payload["value"]["ruleHashesWithFindings"] = {
            r.full_hash: len(f) for r, f in findings.kept.items()
        }
        self.payload["value"]["numFindings"] = sum(
            len(v) for v in findings.kept.values()
        )
        self.payload["value"]["numIgnored"] = sum(
            len(v) for v in findings.removed.values()
        )

    def add_targets(self, targets: Set[Path], profiling_data: ProfilingData) -> None:
        self.payload["performance"]["fileStats"] = [
            {
                "size": target.stat().st_size,
                "numTimesScanned": profiling_data.get_file_num_times_scanned(target),
                "parseTime": profiling_data.get_file_parse_time(target),
                "matchTime": profiling_data.get_file_match_time(target),
                "runTime": profiling_data.get_file_run_time(target),
            }
            for target in targets
        ]

        total_bytes_scanned = sum(t.stat().st_size for t in targets)
        self.payload["performance"]["totalBytesScanned"] = total_bytes_scanned
        self.payload["performance"]["numTargets"] = len(targets)

    def add_errors(self, errors: List[SemgrepError]) -> None:
        self.payload["errors"]["errors"] = [e.semgrep_error_type() for e in errors]

    def add_profiling(self, profiler: ProfileManager) -> None:
        self.payload["performance"]["profilingTimes"] = profiler.dump_stats()

    def add_token(self, token: Optional[str]) -> None:
        self.payload["environment"]["isAuthenticated"] = bool(token)

    def add_exit_code(self, exit_code: int) -> None:
        self.payload["errors"]["returnCode"] = exit_code

    def add_version(self, version: str) -> None:
        self.payload["environment"]["version"] = version

    def as_json(self) -> str:
        return json.dumps(
            self.payload, indent=2, sort_keys=True, cls=MetricsJsonEncoder
        )

    @property
    def is_enabled(self) -> bool:
        """
        Returns whether metrics should be sent.

        If metrics_state is:
          - auto, sends if using_registry
          - on, sends
          - off, doesn't send
        """
        if self.metrics_state == MetricsState.AUTO:
            return self.is_using_registry
        return self.metrics_state == MetricsState.ON

    def send(self) -> None:
        """
        Send metrics to the metrics server.

        Will if is_enabled is True
        """
        from semgrep.state import get_state  # avoiding circular import

        state = get_state()
        logger.verbose(
            f"{'Sending' if self.is_enabled else 'Not sending'} pseudonymous metrics since metrics are configured to {self.metrics_state.name} and registry usage is {self.is_using_registry}"
        )

        if not self.is_enabled:
            return

        self.payload["sent_at"] = datetime.now()
        self.payload["anonymous_user_id"] = state.settings.get("anonymous_user_id")

        try:
            r = requests.post(
                METRICS_ENDPOINT,
                data=self.as_json(),
                headers={
                    "Content-Type": "application/json",
                    "User-Agent": str(state.app_session.user_agent),
                },
                timeout=3,
            )
            r.raise_for_status()
            logger.debug("Sent pseudonymous metrics")
        except Exception as e:
            logger.debug(f"Failed to send pseudonymous metrics: {e}")
