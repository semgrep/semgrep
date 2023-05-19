import functools
import hashlib
import json
import os
import uuid
from datetime import datetime
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Any
from typing import Callable
from typing import List
from typing import NewType
from typing import Optional
from typing import Sequence
from typing import Set
from typing import TYPE_CHECKING
from urllib.parse import urlparse

import click
import requests
from attr import define
from attr import Factory
from typing_extensions import LiteralString

from semgrep import __VERSION__
from semgrep.error import SemgrepError
from semgrep.parsing_data import ParsingData
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.rule import Rule
from semgrep.semgrep_interfaces.semgrep_metrics import Datetime
from semgrep.semgrep_interfaces.semgrep_metrics import Environment
from semgrep.semgrep_interfaces.semgrep_metrics import Errors
from semgrep.semgrep_interfaces.semgrep_metrics import Extension
from semgrep.semgrep_interfaces.semgrep_metrics import FileStats
from semgrep.semgrep_interfaces.semgrep_metrics import ParseStat
from semgrep.semgrep_interfaces.semgrep_metrics import Payload
from semgrep.semgrep_interfaces.semgrep_metrics import Performance
from semgrep.semgrep_interfaces.semgrep_metrics import RuleStats
from semgrep.semgrep_interfaces.semgrep_metrics import Sha256hash
from semgrep.semgrep_interfaces.semgrep_metrics import Uuid
from semgrep.semgrep_interfaces.semgrep_metrics import Value
from semgrep.types import FilteredMatches
from semgrep.verbose_logging import getLogger

if TYPE_CHECKING:
    from semgrep.engine import EngineType

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


class MetricsJsonEncoder(json.JSONEncoder):
    def default(self, obj: Any) -> Any:
        if isinstance(obj, datetime):
            return obj.astimezone().isoformat()

        if isinstance(obj, uuid.UUID):
            return str(obj)

        if isinstance(obj, set):
            return list(sorted(obj))

        return super().default(obj)


def suppress_errors(func: Callable[..., None]) -> Callable[..., None]:
    @functools.wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        try:
            return func(*args, **kwargs)
        except Exception as e:
            logger.debug(f"Error in {func.__name__}: {e}")
            return None

    return wrapper


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
    payload: Payload = Factory(
        lambda: Payload(
            environment=Environment(
                version=__VERSION__,
                configNamesHash=Sha256hash(""),
                projectHash=None,
                ci=None,
            ),
            errors=Errors(),
            performance=Performance(maxMemoryBytes=None),
            extension=Extension(),
            value=Value(features=[]),
            started_at=Datetime(datetime.now().astimezone().isoformat()),
            event_id=Uuid(str(uuid.uuid4())),
            anonymous_user_id="",
            parse_rate=[],
            sent_at=Datetime(""),
        )
    )

    def __attrs_post_init__(self) -> None:
        self.payload.environment.ci = os.getenv("CI")

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

    @suppress_errors
    def add_engine_type(self, engineType: "EngineType") -> None:
        """
        Assumes configs is list of arguments passed to semgrep using --config
        """
        self.payload.value.engineRequested = engineType.name

    @property
    def is_using_registry(self) -> bool:
        return self._is_using_registry

    @is_using_registry.setter
    def is_using_registry(self, value: bool) -> None:
        self._is_using_registry = value

    @suppress_errors
    def add_project_url(self, project_url: Optional[str]) -> None:
        """
        Standardizes url then hashes
        """
        if project_url is None:
            self.payload.environment.projectHash = None
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
        self.payload.environment.projectHash = Sha256hash(m.hexdigest())

    @suppress_errors
    def add_configs(self, configs: Sequence[str]) -> None:
        """
        Assumes configs is list of arguments passed to semgrep using --config
        """
        m = hashlib.sha256()
        for c in configs:
            m.update(c.encode())
        self.payload.environment.configNamesHash = Sha256hash(m.hexdigest())

    @suppress_errors
    def add_rules(self, rules: Sequence[Rule], profiling_data: ProfilingData) -> None:
        rules = sorted(rules, key=lambda r: r.full_hash)
        m = hashlib.sha256()
        for rule in rules:
            m.update(rule.full_hash.encode())
        self.payload.environment.rulesHash = Sha256hash(m.hexdigest())

        self.payload.performance.numRules = len(rules)
        self.payload.performance.ruleStats = [
            RuleStats(
                ruleHash=rule.full_hash,
                matchTime=profiling_data.get_rule_match_time(rule),
                bytesScanned=profiling_data.get_rule_bytes_scanned(rule),
            )
            for rule in rules
        ]

    @suppress_errors
    def add_max_memory_bytes(self, profiling_data: ProfilingData) -> None:
        self.payload.performance.maxMemoryBytes = profiling_data.get_max_memory_bytes()

    @suppress_errors
    def add_findings(self, findings: FilteredMatches) -> None:
        self.payload.value.ruleHashesWithFindings = [
            (r.full_hash, len(f)) for r, f in findings.kept.items()
        ]
        self.payload.value.numFindings = sum(len(v) for v in findings.kept.values())
        self.payload.value.numIgnored = sum(len(v) for v in findings.removed.values())

    @suppress_errors
    def add_targets(self, targets: Set[Path], profiling_data: ProfilingData) -> None:
        self.payload.performance.fileStats = [
            FileStats(
                size=target.stat().st_size,
                numTimesScanned=profiling_data.get_file_num_times_scanned(target),
                parseTime=profiling_data.get_file_parse_time(target),
                matchTime=profiling_data.get_file_match_time(target),
                runTime=profiling_data.get_file_run_time(target),
            )
            for target in targets
        ]

        total_bytes_scanned = sum(t.stat().st_size for t in targets)
        self.payload.performance.totalBytesScanned = total_bytes_scanned
        self.payload.performance.numTargets = len(targets)

    @suppress_errors
    def add_errors(self, errors: List[SemgrepError]) -> None:
        self.payload.errors.errors = [e.semgrep_error_type() for e in errors]

    @suppress_errors
    def add_profiling(self, profiler: ProfileManager) -> None:
        self.payload.performance.profilingTimes = [
            (k, v) for k, v in profiler.dump_stats().items()
        ]

    @suppress_errors
    def add_token(self, token: Optional[str]) -> None:
        self.payload.environment.isAuthenticated = bool(token)

    @suppress_errors
    def add_integration_name(self, name: Optional[str]) -> None:
        self.payload.environment.integrationName = name

    @suppress_errors
    def add_exit_code(self, exit_code: int) -> None:
        self.payload.errors.returnCode = exit_code

    @suppress_errors
    def add_version(self, version: str) -> None:
        self.payload.environment.version = version

    @suppress_errors
    def add_feature(self, category: LiteralString, name: str) -> None:
        self.payload.value.features.append(f"{category}/{name}")
        self.payload.value.features.sort()

    @suppress_errors
    def add_registry_url(self, url_string: str) -> None:
        path = urlparse(url_string).path
        parts = path.lstrip("/").split("/")
        if len(parts) != 2:
            return  # not a simple registry shorthand

        prefix, name = parts

        if prefix == "r":
            # we want to avoid reporting specific rules, so we do this mapping:
            # r/python -> "python"
            # r/python.flask -> "python."
            # r/python.correctness.lang => "python.."
            query_parts = name.split(".")
            dot_count = len(query_parts) - 1
            self.add_feature("registry-query", query_parts[0] + dot_count * ".")
        if prefix == "p":
            self.add_feature("ruleset", name)

    @suppress_errors
    def add_parse_rates(self, parse_rates: ParsingData) -> None:
        """
        Adds parse rates, grouped by language
        """
        self.payload.parse_rate = [
            (
                str(lang),
                ParseStat(
                    targets_parsed=data.num_targets - data.targets_with_errors,
                    num_targets=data.num_targets,
                    bytes_parsed=data.num_bytes - data.error_bytes,
                    num_bytes=data.num_bytes,
                ),
            )
            for (lang, data) in parse_rates.get_errors_by_lang().items()
        ]

    @suppress_errors
    def add_extension(
        self,
        machine_id: Optional[str],
        new_install: Optional[bool],
        session_id: Optional[str],
        version: Optional[str],
        type: Optional[str],
    ) -> None:
        self.payload.extension = Extension(
            machineId=machine_id,
            isNewAppInstall=new_install,
            sessionId=session_id,
            version=version,
            ty=type,
        )

    def as_json(self) -> str:
        value = self.payload.to_json()
        return json.dumps(value, indent=2, sort_keys=True, cls=MetricsJsonEncoder)

    @property
    def is_enabled(self) -> bool:
        """
        Returns whether metrics should be sent.

        If metrics_state is:
          - auto, sends if using_registry
          - on, sends
          - off, doesn't send
        """
        # import here to prevent circular import
        from semgrep.state import get_state

        state = get_state()

        if self.metrics_state == MetricsState.AUTO:
            # When running logged in with `semgrep ci`, configs are
            # resolved before `self.is_using_registry` is set.
            # However, these scans are still pulling from the registry
            using_app = (
                state.command.get_subcommand() == "ci"
                and state.app_session.is_authenticated
            )
            return self.is_using_registry or using_app
        return self.metrics_state == MetricsState.ON

    @suppress_errors
    def gather_click_params(self) -> None:
        ctx = click.get_current_context()
        if ctx is None:
            return
        for param in ctx.params:
            source = ctx.get_parameter_source(param)
            if source == click.core.ParameterSource.COMMANDLINE:
                self.add_feature("cli-flag", param)
            if source == click.core.ParameterSource.ENVIRONMENT:
                self.add_feature("cli-envvar", param)
            if source == click.core.ParameterSource.PROMPT:
                self.add_feature("cli-prompt", param)

    # Posting the metrics is separated out so that our tests can check
    # for it
    # TODO it's a bit unfortunate that our tests are going to post
    # metrics...
    def _post_metrics(self, user_agent: str) -> None:
        r = requests.post(
            METRICS_ENDPOINT,
            data=self.as_json(),
            headers={
                "Content-Type": "application/json",
                "User-Agent": user_agent,
            },
            timeout=3,
        )
        r.raise_for_status()

    @suppress_errors
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

        self.gather_click_params()
        self.payload.sent_at = Datetime(datetime.now().astimezone().isoformat())
        self.payload.anonymous_user_id = state.settings.get("anonymous_user_id")

        self._post_metrics(str(state.app_session.user_agent))
