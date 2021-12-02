import contextlib
import pathlib
import sys
from collections import defaultdict
from pathlib import Path
from typing import Any
from typing import Collection
from typing import Dict
from typing import FrozenSet
from typing import Generator
from typing import IO
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Type

from semgrep import config_resolver
from semgrep.constants import OutputFormat
from semgrep.constants import RuleSeverity
from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import Level
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.formatter.emacs import EmacsFormatter
from semgrep.formatter.json import JsonFormatter
from semgrep.formatter.junit_xml import JunitXmlFormatter
from semgrep.formatter.sarif import SarifFormatter
from semgrep.formatter.text import TextFormatter
from semgrep.formatter.vim import VimFormatter
from semgrep.profile_manager import ProfileManager
from semgrep.profiling import ProfilingData
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match_map import RuleMatchMap
from semgrep.stats import make_loc_stats
from semgrep.stats import make_target_stats
from semgrep.util import is_url
from semgrep.util import terminal_wrap
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


FORMATTERS: Mapping[OutputFormat, Type[BaseFormatter]] = {
    OutputFormat.EMACS: EmacsFormatter,
    OutputFormat.JSON: JsonFormatter,
    OutputFormat.JUNIT_XML: JunitXmlFormatter,
    OutputFormat.SARIF: SarifFormatter,
    OutputFormat.TEXT: TextFormatter,
    OutputFormat.VIM: VimFormatter,
}

DEFAULT_SHOWN_SEVERITIES: Collection[RuleSeverity] = frozenset(
    {RuleSeverity.INFO, RuleSeverity.WARNING, RuleSeverity.ERROR}
)


def get_path_str(target: Path) -> str:
    path_str = ""
    try:
        path_str = str(target.relative_to(pathlib.Path().absolute()))
    except ValueError:
        path_str = str(target)
    return path_str


def _build_time_target_json(
    rules: List[Rule],
    target: Path,
    num_bytes: int,
    profiling_data: ProfilingData,
) -> Dict[str, Any]:
    target_json: Dict[str, Any] = {}
    path_str = get_path_str(target)

    target_json["path"] = path_str
    target_json["num_bytes"] = num_bytes
    timings = [profiling_data.get_run_times(rule, target) for rule in rules]
    target_json["parse_times"] = [timing.parse_time for timing in timings]
    target_json["match_times"] = [timing.match_time for timing in timings]
    target_json["run_times"] = [timing.run_time for timing in timings]

    return target_json


# coupling: if you change the JSON schema below, you probably need to
# also modify perf/run-benchmarks. Run locally
#    $ ./run-benchmarks --dummy --upload
# to double check everything still works
def _build_time_json(
    rules: List[Rule],
    targets: Set[Path],
    profiling_data: ProfilingData,  # (rule, target) -> times
    profiler: Optional[ProfileManager],
) -> Dict[str, Any]:
    """Convert match times to a json-ready format.

    Match times are obtained for each pair (rule, target) by running
    semgrep-core. They exclude parsing time. One of the applications is
    to estimate the performance of a rule, assuming the cost of parsing
    the target file will become negligible once we run many rules on the
    same AST.
    """
    time_info: Dict[str, Any] = {}
    # this list of all rules names is given here so they don't have to be
    # repeated for each target in the 'targets' field, saving space.
    time_info["rules"] = [{"id": rule.id} for rule in rules]
    time_info["rule_parse_info"] = [
        profiling_data.get_rule_parse_time(rule) for rule in rules
    ]
    time_info["profiling_times"] = profiler.dump_stats() if profiler else {}
    target_bytes = [Path(str(target)).resolve().stat().st_size for target in targets]
    time_info["targets"] = [
        _build_time_target_json(rules, target, num_bytes, profiling_data)
        for target, num_bytes in zip(targets, target_bytes)
    ]
    time_info["total_bytes"] = sum(n for n in target_bytes)
    return time_info


# WARNING: this class is unofficially part of our external API. It can be passed
# as an argument to our official API: 'semgrep_main.invoke_semgrep'. Try to minimize
# changes to this API, and make them backwards compatible, if possible.
class OutputSettings(NamedTuple):
    output_format: OutputFormat
    output_destination: Optional[str] = None
    output_per_finding_max_lines_limit: Optional[int] = None
    output_per_line_max_chars_limit: Optional[int] = None
    error_on_findings: bool = False
    verbose_errors: bool = False  # to do: rename to just 'verbose'
    strict: bool = False
    debug: bool = False
    json_stats: bool = False
    output_time: bool = False
    timeout_threshold: int = 0


@contextlib.contextmanager
def managed_output(output_settings: OutputSettings) -> Generator:
    """
    Context manager to capture uncaught exceptions &
    """
    output_handler = OutputHandler(output_settings)
    try:
        yield output_handler
    except Exception as ex:
        output_handler.handle_unhandled_exception(ex)
    finally:
        output_handler.close()


class OutputHandler:
    """
    Handle all output in a central location. Rather than calling `print_stderr` directly,
    you should call `handle_*` as appropriate.

    In normal usage, it should be constructed via the contextmanager, `managed_output`. It ensures that everything
    is handled properly if exceptions are thrown.

    If you need to stop execution immediately (think carefully if you really want this!), throw an exception.
    If this is normal behavior, the exception _must_ inherit from `SemgrepError`.

    If you want execution to continue, _report_ the exception via the appropriate `handle_*` method.
    """

    def __init__(
        self,
        output_settings: OutputSettings,
        stderr: IO = sys.stderr,
        stdout: IO = sys.stdout,
    ):
        self.settings = output_settings
        self.stderr = stderr
        self.stdout = stdout

        self.rule_matches: List[RuleMatch] = []
        self.stats_line: Optional[str] = None
        self.all_targets: Set[Path] = set()
        self.profiler: Optional[ProfileManager] = None
        self.rules: FrozenSet[Rule] = frozenset()
        self.semgrep_structured_errors: List[SemgrepError] = []
        self.error_set: Set[SemgrepError] = set()
        self.has_output = False
        self.filtered_rules: List[Rule] = []
        self.profiling_data: ProfilingData = (
            ProfilingData()
        )  # (rule, target) -> duration
        self.severities: Collection[RuleSeverity] = DEFAULT_SHOWN_SEVERITIES

        self.final_error: Optional[Exception] = None
        formatter_type = FORMATTERS.get(self.settings.output_format)
        if formatter_type is None:
            raise RuntimeError(f"Invalid output format: {self.settings.output_format}")

        self.formatter = formatter_type()

    def handle_semgrep_errors(self, errors: Sequence[SemgrepError]) -> None:
        timeout_errors = defaultdict(list)
        for err in errors:
            if (
                isinstance(err, SemgrepCoreError)
                and err.is_timeout()
                and err not in self.error_set
            ):
                self.semgrep_structured_errors.append(err)
                self.error_set.add(err)
                assert err.rule_id  # Always defined for timeout errors
                timeout_errors[err.path].append(err.rule_id)
            else:
                self.handle_semgrep_error(err)

        if timeout_errors and self.settings.output_format == OutputFormat.TEXT:
            t_errors = dict(timeout_errors)  # please mypy
            self.handle_semgrep_timeout_errors(t_errors)

    def handle_semgrep_timeout_errors(self, errors: Dict[Path, List[str]]) -> None:
        self.has_output = True
        separator = ", "
        print_threshold_hint = False
        for path in errors.keys():
            num_errs = len(errors[path])
            errors[path].sort()
            error_msg = f"Warning: {num_errs} timeout error(s) in {path} when running the following rules: [{separator.join(errors[path])}]"
            if num_errs == self.settings.timeout_threshold:
                error_msg += f"\nSemgrep stopped running rules on {path} after {num_errs} timeout error(s). See `--timeout-threshold` for more info."
            print_threshold_hint = print_threshold_hint or (
                num_errs > 5 and not self.settings.timeout_threshold
            )
            logger.error(with_color("red", terminal_wrap(error_msg)))

        if print_threshold_hint:
            logger.error(
                with_color(
                    "red",
                    f"You can use the `--timeout-threshold` flag to set a number of timeouts after which a file will be skipped.",
                )
            )

    def handle_semgrep_error(self, error: SemgrepError) -> None:
        """
        Reports generic exceptions that extend SemgrepError
        """
        self.has_output = True
        if error not in self.error_set:
            self.semgrep_structured_errors.append(error)
            self.error_set.add(error)
            if self.settings.output_format == OutputFormat.TEXT and (
                error.level != Level.WARN or self.settings.verbose_errors
            ):
                logger.error(with_color("red", str(error)))

    def handle_semgrep_core_output(
        self,
        rule_matches_by_rule: RuleMatchMap,
        *,
        stats_line: str,
        all_targets: Set[Path],
        profiler: ProfileManager,
        filtered_rules: List[Rule],
        profiling_data: ProfilingData,  # (rule, target) -> duration
        severities: Optional[Collection[RuleSeverity]],
    ) -> None:
        self.has_output = True
        self.rules = self.rules.union(rule_matches_by_rule.keys())
        self.rule_matches += [
            match
            for matches_of_one_rule in rule_matches_by_rule.values()
            for match in matches_of_one_rule
        ]
        self.profiler = profiler
        self.all_targets = all_targets
        self.stats_line = stats_line
        self.filtered_rules = filtered_rules
        self.profiling_data = profiling_data
        if severities:
            self.severities = severities

    def handle_unhandled_exception(self, ex: Exception) -> None:
        """
        This is called by the context manager upon an unhandled exception. If you want to record a final
        error & set the exit code, but keep executing to perform cleanup tasks, call this method.
        """

        if isinstance(ex, SemgrepError):
            self.handle_semgrep_error(ex)
        self.final_error = ex

    def final_raise(self, ex: Optional[Exception], error_stats: Optional[str]) -> None:
        if ex is None:
            return
        if isinstance(ex, SemgrepError):
            if ex.level == Level.ERROR:
                raise ex
            else:
                if self.settings.strict:
                    raise ex
                logger.info(
                    terminal_wrap(
                        f"{error_stats}; run with --verbose for details or run with --strict to exit non-zero if any file cannot be analyzed cleanly"
                    )
                )
        else:
            raise ex

    def close(self) -> None:
        """
        Close the output handler.

        This will write any output that hasn't been written so far. It returns
        the exit code of the program.
        """
        if self.has_output:
            output = self.build_output(
                self.settings.output_destination is None and self.stdout.isatty(),
                self.settings.output_per_finding_max_lines_limit,
                self.settings.output_per_line_max_chars_limit,
            )
            if self.settings.output_destination:
                self.save_output(self.settings.output_destination, output)
            else:
                if output:
                    try:
                        print(output, file=self.stdout)
                    except UnicodeEncodeError as ex:
                        raise Exception(
                            "Received output encoding error, please set PYTHONIOENCODING=utf-8"
                        ) from ex
            if self.stats_line:
                logger.info(self.stats_line)

        final_error = None
        error_stats = None
        any_findings_not_ignored = any(not rm.is_ignored for rm in self.rule_matches)
        if self.final_error:
            final_error = self.final_error
        elif any_findings_not_ignored and self.settings.error_on_findings:
            # This exception won't be visible to the user, we're just
            # using this to return a specific error code
            final_error = SemgrepError("", code=FINDINGS_EXIT_CODE)
        elif self.semgrep_structured_errors:
            # make a simplifying assumption that # errors = # files failed
            # it's a quite a bit of work to simplify further because errors may or may not have path, span, etc.
            num_errors = len(self.semgrep_structured_errors)
            plural = "s" if num_errors > 1 else ""
            error_stats = f"found problems analyzing {num_errors} file{plural}"
            final_error = self.semgrep_structured_errors[-1]
        self.final_raise(final_error, error_stats)

    @classmethod
    def save_output(cls, destination: str, output: str) -> None:
        if is_url(destination):
            cls.post_output(destination, output)
        else:
            if Path(destination).is_absolute():
                save_path = Path(destination)
            else:
                base_path = config_resolver.get_base_path()
                save_path = base_path.joinpath(destination)
            # create the folders if not exists
            save_path.parent.mkdir(parents=True, exist_ok=True)
            with save_path.open(mode="w") as fout:
                fout.write(output)

    @classmethod
    def post_output(cls, output_url: str, output: str) -> None:
        import requests  # here for faster startup times

        logger.info(f"posting to {output_url}...")
        try:
            r = requests.post(output_url, data=output, timeout=10)
            logger.verbose(
                f"posted to {output_url} and got status_code:{r.status_code}"
            )
        except requests.exceptions.Timeout:
            raise SemgrepError(f"posting output to {output_url} timed out")

    def build_output(
        self,
        color_output: bool,
        per_finding_max_lines_limit: Optional[int],
        per_line_max_chars_limit: Optional[int],
    ) -> str:
        extra: Dict[str, Any] = {}
        if self.settings.json_stats:
            extra["stats"] = {
                "targets": make_target_stats(self.all_targets),
                "loc": make_loc_stats(self.all_targets),
                "profiler": self.profiler.dump_stats() if self.profiler else None,
            }
        if self.settings.output_time:
            extra["time"] = _build_time_json(
                self.filtered_rules,
                self.all_targets,
                self.profiling_data,
                self.profiler,
            )
        if self.settings.output_format == OutputFormat.TEXT:
            extra["color_output"] = color_output
            extra["per_finding_max_lines_limit"] = per_finding_max_lines_limit
            extra["per_line_max_chars_limit"] = per_line_max_chars_limit

        return self.formatter.output(
            self.rules,
            self.rule_matches,
            self.semgrep_structured_errors,
            extra,
            self.severities,
        )
