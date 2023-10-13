import dataclasses
import pathlib
import sys
from collections import defaultdict
from functools import reduce
from pathlib import Path
from typing import Any
from typing import Collection
from typing import Dict
from typing import FrozenSet
from typing import List
from typing import Mapping
from typing import NamedTuple
from typing import Optional
from typing import Sequence
from typing import Set
from typing import Tuple
from typing import Type

import requests
from boltons.iterutils import partition

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.console import console
from semgrep.console import Title
from semgrep.constants import Colors
from semgrep.constants import OutputFormat
from semgrep.engine import EngineType
from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.formatter.emacs import EmacsFormatter
from semgrep.formatter.gitlab_sast import GitlabSastFormatter
from semgrep.formatter.gitlab_secrets import GitlabSecretsFormatter
from semgrep.formatter.json import JsonFormatter
from semgrep.formatter.junit_xml import JunitXmlFormatter
from semgrep.formatter.sarif import SarifFormatter
from semgrep.formatter.text import TextFormatter
from semgrep.formatter.vim import VimFormatter
from semgrep.output_extra import OutputExtra
from semgrep.profile_manager import ProfileManager
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.state import get_state
from semgrep.target_manager import FileTargetingLog
from semgrep.target_manager import TargetManager
from semgrep.util import is_url
from semgrep.util import terminal_wrap
from semgrep.util import unit_str
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


FORMATTERS: Mapping[OutputFormat, Type[BaseFormatter]] = {
    OutputFormat.EMACS: EmacsFormatter,
    OutputFormat.GITLAB_SAST: GitlabSastFormatter,
    OutputFormat.GITLAB_SECRETS: GitlabSecretsFormatter,
    OutputFormat.JSON: JsonFormatter,
    OutputFormat.JUNIT_XML: JunitXmlFormatter,
    OutputFormat.SARIF: SarifFormatter,
    OutputFormat.TEXT: TextFormatter,
    OutputFormat.VIM: VimFormatter,
}

DEFAULT_SHOWN_SEVERITIES: Collection[out.MatchSeverity] = frozenset(
    {
        out.MatchSeverity(out.Info()),
        out.MatchSeverity(out.Warning()),
        out.MatchSeverity(out.Error()),
    }
)


def get_path_str(target: Path) -> str:
    path_str = ""
    try:
        path_str = str(target.relative_to(pathlib.Path().absolute()))
    except ValueError:
        path_str = str(target)
    return path_str


def _build_time_json(
    rules: List[Rule],
    targets: Set[Path],
    profile: out.Profile,
    profiler: Optional[ProfileManager],
) -> out.Profile:
    # TODO: we used to start from the targets and rules passed as a parameter
    # and then grab the information in profile, but
    # now we just reuse profile without any processing.
    # Can things differ between the targets/rules in pysemgrep and the
    # one actually used in semgrep-core and returned in profile?

    return out.Profile(
        # this is an addon to profiling_data.profile
        profiling_times=profiler.dump_stats() if profiler else {},
        # TODO: maybe just start from profiling_data.profile and just adjust its
        # profiling_times field
        rules=profile.rules,
        targets=profile.targets,
        total_bytes=profile.total_bytes,
        rules_parse_time=profile.rules_parse_time,
        max_memory_bytes=profile.max_memory_bytes,
    )


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
    output_time: bool = False
    timeout_threshold: int = 0
    dataflow_traces: bool = False


class OutputHandler:
    """
    Handle all output in a central location. Rather than calling `print_stderr` directly,
    you should call `handle_*` as appropriate.

    In normal usage, it should be constructed via the contextmanager, `managed_output`.
    It ensures that everything is handled properly if exceptions are thrown.

    If you need to stop execution immediately (think carefully if you really want this!),
    throw an exception.
    If this is normal behavior, the exception _must_ inherit from `SemgrepError`.

    If you want execution to continue, _report_ the exception via the appropriate
    `handle_*` method.
    """

    def __init__(
        self,
        output_settings: OutputSettings,
    ):
        self.settings = output_settings

        self.rule_matches: List[RuleMatch] = []
        self.all_targets: Set[Path] = set()
        self.profiler: Optional[ProfileManager] = None
        self.rules: FrozenSet[Rule] = frozenset()
        self.semgrep_structured_errors: List[SemgrepError] = []
        self.error_set: Set[SemgrepError] = set()
        self.has_output = False
        self.is_ci_invocation = False
        self.filtered_rules: List[Rule] = []
        self.extra: Optional[OutputExtra] = None
        self.severities: Collection[out.MatchSeverity] = DEFAULT_SHOWN_SEVERITIES
        self.explanations: Optional[List[out.MatchingExplanation]] = None
        self.engine_type: EngineType = EngineType.OSS

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

                if not err.core.rule_id:
                    timeout_errors[Path(err.core.location.path.value)].append(
                        "<unknown rule_id>"
                    )
                else:
                    timeout_errors[Path(err.core.location.path.value)].append(
                        err.core.rule_id.value
                    )
            else:
                self._handle_semgrep_error(err)

        if timeout_errors and self.settings.output_format == OutputFormat.TEXT:
            t_errors = dict(timeout_errors)  # please mypy
            self._handle_semgrep_timeout_errors(t_errors)

    def _handle_semgrep_timeout_errors(self, errors: Dict[Path, List[str]]) -> None:
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
            logger.error(with_color(Colors.red, terminal_wrap(error_msg)))

        if print_threshold_hint:
            logger.error(
                with_color(
                    Colors.red,
                    f"You can use the `--timeout-threshold` flag to set a number of timeouts after which a file will be skipped.",
                )
            )

    def _handle_semgrep_error(self, error: SemgrepError) -> None:
        """
        Reports generic exceptions that extend SemgrepError
        """
        self.has_output = True
        if error not in self.error_set:
            self.semgrep_structured_errors.append(error)
            self.error_set.add(error)
            if self.settings.output_format == OutputFormat.TEXT and (
                not (isinstance(error.level.value, out.Warning_))
                or self.settings.verbose_errors
            ):
                logger.error(error.format_for_terminal())

    def _final_raise(self, ex: Optional[Exception]) -> None:
        if ex is None:
            return
        if isinstance(ex, SemgrepError):
            if isinstance(ex.level.value, out.Error_) and not (
                isinstance(ex, SemgrepCoreError)
                and ex.is_special_interfile_analysis_error
            ):
                raise ex
            elif self.settings.strict:
                raise ex
        else:
            raise ex

    @staticmethod
    def _make_failed_to_analyze(
        semgrep_core_errors: Sequence[SemgrepCoreError],
    ) -> Mapping[Path, Tuple[Optional[int], List[out.RuleId]]]:
        def update_failed_to_analyze(
            memo: Mapping[Path, Tuple[Optional[int], List[out.RuleId]]],
            err: SemgrepCoreError,
        ) -> Mapping[Path, Tuple[Optional[int], List[out.RuleId]]]:
            path = Path(err.core.location.path.value)
            so_far = memo.get(path, (0, []))
            if err.spans is None or so_far[0] is None:
                num_lines = None
            else:
                num_lines = so_far[0] + sum(
                    s.end.line - s.start.line + 1 for s in err.spans
                )
            rule_ids = so_far[1]
            if err.core.rule_id is not None:
                rule_ids.append(err.core.rule_id)

            return {**memo, path: (num_lines, rule_ids)}

        return reduce(update_failed_to_analyze, semgrep_core_errors, {})

    # TODO: why run_scan.scan() calls output() to set the fields why
    # run_scan.run_scan_and_return_json() modify directly the fields instead?
    def output(
        self,
        rule_matches_by_rule: RuleMatchMap,
        *,
        all_targets: Set[Path],
        filtered_rules: List[Rule],
        ignore_log: Optional[FileTargetingLog] = None,
        profiler: Optional[ProfileManager] = None,
        extra: Optional[OutputExtra] = None,
        explanations: Optional[List[out.MatchingExplanation]] = None,
        severities: Optional[Collection[out.MatchSeverity]] = None,
        print_summary: bool = False,
        is_ci_invocation: bool = False,
        engine_type: EngineType = EngineType.OSS,
    ) -> None:
        state = get_state()
        self.has_output = True
        self.rules = self.rules.union(rule_matches_by_rule.keys())
        self.rule_matches = [
            match
            for matches_of_one_rule in rule_matches_by_rule.values()
            for match in matches_of_one_rule
        ]
        self.profiler = profiler
        self.all_targets = all_targets
        self.filtered_rules = filtered_rules

        self.engine_type = engine_type

        if ignore_log:
            self.ignore_log = ignore_log
        else:
            # ignore log was not created, so the run failed before it even started
            # create a fake log to track the errors
            self.ignore_log = FileTargetingLog(TargetManager(["."]))

        if extra:
            self.extra = extra
        if explanations:
            self.explanations = explanations
        if severities:
            self.severities = severities

        self.is_ci_invocation = is_ci_invocation

        final_error = None
        any_findings_not_ignored = any(not rm.is_ignored for rm in self.rule_matches)

        if self.final_error:
            final_error = self.final_error
        elif any_findings_not_ignored and self.settings.error_on_findings:
            # This exception won't be visible to the user, we're just
            # using this to return a specific error code
            final_error = SemgrepError("", code=FINDINGS_EXIT_CODE)
        elif self.semgrep_structured_errors:
            # Assumption: only the semgrep core errors pertain to files; if there are other
            # errors, they didn't affect the whether files were analyzed, but were a different
            # kind of error (for example, baseline commit not found)
            semgrep_core_errors = [
                err
                for err in self.semgrep_structured_errors
                if isinstance(err, SemgrepCoreError)
            ]

            failed_to_analyze_lines_by_path = self._make_failed_to_analyze(
                semgrep_core_errors
            )
            final_error = self.semgrep_structured_errors[-1]
            self.ignore_log.core_failure_lines_by_file = failed_to_analyze_lines_by_path

        if self.has_output:
            output = self._build_output()
            if self.settings.output_destination:
                self._save_output(self.settings.output_destination, output)
            else:
                if output:
                    try:
                        # console.print() would go to stderr; here we print() directly to stdout
                        # the output string is already pre-formatted by semgrep.console
                        print(output)
                    except UnicodeEncodeError as ex:
                        raise Exception(
                            "Received output encoding error, please set PYTHONIOENCODING=utf-8"
                        ) from ex

        if self.filtered_rules:
            fingerprint_matches, regular_matches = partition(
                self.rule_matches,
                lambda m: m.severity
                in [
                    out.MatchSeverity(out.Inventory()),
                    out.MatchSeverity(out.Experiment()),
                ],
            )
            num_findings = len(regular_matches)
            num_targets = len(self.all_targets)
            num_rules = len(self.filtered_rules)

            ignores_line = str(ignore_log or "No ignore information available")
            suggestion_line = ""
            if (
                num_findings == 0
                and num_targets > 0
                and num_rules > 0
                and state.metrics.is_using_registry
                and state.app_session.token is None
            ):
                suggestion_line = "\n(need more rules? `semgrep login` for additional free Semgrep Registry rules)\n"
            stats_line = ""
            if print_summary:
                stats_line = f"\nRan {unit_str(num_rules, 'rule')} on {unit_str(num_targets, 'file')}: {unit_str(num_findings, 'finding')}."
            if ignore_log is not None:
                logger.verbose(ignore_log.verbose_output())

            output_text = ignores_line + suggestion_line + stats_line
            console.print(Title("Scan Summary"))
            logger.info(output_text)

        self._final_raise(final_error)

    def _save_output(self, destination: str, output: str) -> None:
        metrics = get_state().metrics
        if is_url(destination):
            metrics.add_feature("output", "url")
            self._post_output(destination, output)
        else:
            metrics.add_feature("output", "path")
            save_path = Path(destination)
            # create the folders if not exists
            save_path.parent.mkdir(parents=True, exist_ok=True)
            with save_path.open(mode="w") as fout:
                fout.write(output)

    def _post_output(self, output_url: str, output: str) -> None:
        logger.info(f"posting to {output_url}...")
        try:
            r = requests.post(output_url, data=output, timeout=10)
            logger.verbose(
                f"posted to {output_url} and got status_code:{r.status_code}"
            )
        except requests.exceptions.Timeout:
            raise SemgrepError(f"posting output to {output_url} timed out")

    def _build_output(self) -> str:
        # CliOutputExtra members
        cli_paths = out.ScannedAndSkipped(
            # This is incorrect when some rules are skipped by semgrep-core
            # e.g. proprietary rules.
            # TODO: Use what semgrep-core returns for 'scanned' and 'skipped'.
            scanned=[out.Fpath(str(path)) for path in sorted(self.all_targets)],
            skipped=None,
        )
        cli_timing: Optional[out.Profile] = None

        explanations: Optional[List[out.MatchingExplanation]] = self.explanations

        # Extra, extra! This just in! 🗞️
        # The extra dict is for blatantly skipping type checking and function signatures.
        # - The text formatter uses it to store settings
        # You should use CliOutputExtra for better type checking
        extra: Dict[str, Any] = {}
        if self.settings.output_time and self.extra and self.extra.core.time:
            cli_timing = _build_time_json(
                self.filtered_rules,
                self.all_targets,
                self.extra.core.time,
                self.profiler,
            )
        if self.settings.verbose_errors:
            # TODO: use SkippedTarget directly in ignore_log or in yield_json_objects at least
            skipped = sorted(
                self.ignore_log.yield_json_objects(), key=lambda x: Path(x["path"])
            )
            cli_paths = dataclasses.replace(
                cli_paths,
                skipped=[
                    out.SkippedTarget(
                        path=out.Fpath(x["path"]),
                        reason=out.SkipReason.from_json(x["reason"]),
                    )
                    for x in skipped
                ],
            )
            extra["verbose_errors"] = True
        if self.settings.output_format == OutputFormat.TEXT:
            extra["color_output"] = (
                self.settings.output_destination is None and sys.stdout.isatty(),
            )
            extra[
                "per_finding_max_lines_limit"
            ] = self.settings.output_per_finding_max_lines_limit
            extra[
                "per_line_max_chars_limit"
            ] = self.settings.output_per_line_max_chars_limit
            extra["dataflow_traces"] = self.settings.dataflow_traces
        if self.settings.output_format == OutputFormat.SARIF:
            extra["dataflow_traces"] = self.settings.dataflow_traces

        # as opposed to below, we need to distinguish the various kinds of pro engine
        extra["engine_requested"] = self.engine_type

        # TODO: I thought we could guard this code with 'if self.extra:', and raise
        # a SemgrepError otherwise, but it seems that when semgrep got an error
        # (for example in tests/e2e/test_ci.py::test_bad_config),
        # then this code still get called and self.extra is not set but we still want
        # to output things. This is why I have those ugly 'if self.extra' below
        # that possibly return None.

        # the rules are used only by the SARIF formatter
        return self.formatter.output(
            self.rules,
            self.rule_matches,
            self.semgrep_structured_errors,
            out.CliOutputExtra(
                # TODO: almost like self.extra.core.paths, but not there yet
                paths=cli_paths,
                # TODO: almost like self.extra.core.time, but not there yet
                time=cli_timing,
                # TODO: would like t ouse self.extra.core.explanations byt regressions
                explanations=explanations,
                rules_by_engine=self.extra.core.rules_by_engine if self.extra else None,
                # this flattens the information into just distinguishing "pro" and "not-pro"
                engine_requested=self.engine_type.to_engine_kind(),
                # TODO, should just be self.extra.core.skipped_rules
                skipped_rules=[],
            ),
            extra,
            self.severities,
            is_ci_invocation=self.is_ci_invocation,
        )
