import dataclasses
import os
import pathlib
import sys
from collections import defaultdict
from functools import reduce
from pathlib import Path
from typing import Any
from typing import Collection
from typing import Dict
from typing import FrozenSet
from typing import Iterator
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

import semgrep.app.auth as auth
import semgrep.formatter.base as base
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.console import console
from semgrep.console import Title
from semgrep.constants import Colors
from semgrep.constants import OutputFormat
from semgrep.engine import EngineType
from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
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
from semgrep.state import DesignTreatment
from semgrep.state import get_state
from semgrep.target_manager import FileErrorLog
from semgrep.target_manager import FileTargetingLog
from semgrep.target_manager import TargetManager
from semgrep.util import is_url
from semgrep.util import line_count_of_path
from semgrep.util import pretty_print_percentage
from semgrep.util import terminal_wrap
from semgrep.util import unit_str
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


FORMATTERS: Mapping[OutputFormat, Type[base.BaseFormatter]] = {
    OutputFormat.EMACS: EmacsFormatter,
    OutputFormat.GITLAB_SAST: GitlabSastFormatter,
    OutputFormat.GITLAB_SECRETS: GitlabSecretsFormatter,
    OutputFormat.JSON: JsonFormatter,
    OutputFormat.JUNIT_XML: JunitXmlFormatter,
    OutputFormat.SARIF: SarifFormatter,
    OutputFormat.TEXT: TextFormatter,
    OutputFormat.VIM: VimFormatter,
}

# Experiment and Inventory are not below on purpose
DEFAULT_SHOWN_SEVERITIES: Collection[out.MatchSeverity] = frozenset(
    {
        out.MatchSeverity(out.Info()),
        out.MatchSeverity(out.Low()),
        out.MatchSeverity(out.Warning()),
        out.MatchSeverity(out.Medium()),
        out.MatchSeverity(out.Error()),
        out.MatchSeverity(out.High()),
        out.MatchSeverity(out.Critical()),
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


# This class is the internal representation of OutputSettings below.
# Since it is internal it can change as much as necesarry to make
# typechecking more accurate and enforce invariants.
class NormalizedOutputSettings(NamedTuple):
    # Immutable List of OutputDestination x OutputFormat
    outputs: Dict[Optional[str], OutputFormat]
    output_per_finding_max_lines_limit: Optional[int]
    output_per_line_max_chars_limit: Optional[int]
    error_on_findings: bool
    verbose_errors: bool
    strict: bool
    output_time: bool
    timeout_threshold: int
    dataflow_traces: bool
    # alt: put that in terminal.py, which can then be accessed globally
    max_log_list_entries: int

    def get_outputs(self) -> Iterator[Tuple[Optional[str], OutputFormat]]:
        return self.outputs.items().__iter__()

    def has_output_format(self, other: OutputFormat) -> bool:
        return bool(sum(1 for (_, fmt) in self.get_outputs() if other == fmt))

    def has_text_output(self) -> bool:
        return self.has_output_format(OutputFormat.TEXT)


class OutputSettings(NamedTuple):
    outputs: Optional[Dict[Optional[str], OutputFormat]] = None
    output_format: Optional[OutputFormat] = None
    output_destination: Optional[str] = None
    output_per_finding_max_lines_limit: Optional[int] = None
    output_per_line_max_chars_limit: Optional[int] = None
    error_on_findings: bool = False
    verbose_errors: bool = False  # to do: rename to just 'verbose'
    strict: bool = False
    output_time: bool = False
    timeout_threshold: int = 0
    dataflow_traces: bool = False
    max_log_list_entries: int = 0

    def normalize(self) -> NormalizedOutputSettings:
        normalized_outputs: Dict[Optional[str], OutputFormat] = {}
        if self.output_format is None:
            if self.outputs is None:
                raise RuntimeError(f"Invalid output configuration: No output specified")
            normalized_outputs = self.outputs.copy()
        else:
            if self.outputs is not None:
                normalized_outputs = self.outputs.copy()
            if self.output_destination in normalized_outputs:
                raise RuntimeError(
                    "Invalid output configuration: same output destination with multiple formats."
                )
            normalized_outputs[self.output_destination] = self.output_format

        return NormalizedOutputSettings(
            outputs=normalized_outputs,
            output_per_finding_max_lines_limit=self.output_per_finding_max_lines_limit,
            output_per_line_max_chars_limit=self.output_per_line_max_chars_limit,
            error_on_findings=self.error_on_findings,
            verbose_errors=self.verbose_errors,
            strict=self.strict,
            output_time=self.output_time,
            timeout_threshold=self.timeout_threshold,
            dataflow_traces=self.dataflow_traces,
            max_log_list_entries=self.max_log_list_entries,
        )


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
        self.settings: NormalizedOutputSettings = output_settings.normalize()

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

        self._formatters: Dict[Optional[str], base.BaseFormatter] = {}

        for output_destination, output_format in self.settings.get_outputs():
            formatter: Optional[base.BaseFormatter] = None
            if formatter is None:
                formatter_type = FORMATTERS.get(output_format)
                if formatter_type is not None:
                    formatter = formatter_type()
            if formatter is None:
                raise RuntimeError(f"Invalid output format: {output_format}")

            self._formatters[output_destination] = formatter

    def handle_semgrep_errors(self, errors: Sequence[SemgrepError]) -> None:
        timeout_errors = defaultdict(list)
        missing_plugin_errors = []
        for err in errors:
            if (
                isinstance(err, SemgrepCoreError)
                and err.is_timeout()
                and err not in self.error_set
            ):
                assert err.core.location
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
            elif (
                isinstance(err, SemgrepCoreError)
                and err.is_missing_plugin()
                and err not in self.error_set
            ):
                # THINK: These perhaps should not be errors but reported as "skipped"
                self.semgrep_structured_errors.append(err)
                self.error_set.add(err)

                if err.core.rule_id:
                    missing_plugin_errors.append(err.core.rule_id.value)
            else:
                self._handle_semgrep_error(err)

        if timeout_errors and self.settings.has_text_output():
            t_errors = dict(timeout_errors)  # please mypy
            self._handle_semgrep_timeout_errors(t_errors)

        if missing_plugin_errors and self.settings.has_text_output():
            self._handle_semgrep_missing_plugin_errors(missing_plugin_errors)

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

    def _handle_semgrep_missing_plugin_errors(self, errors: List[str]) -> None:
        self.has_output = True
        num_errs = len(errors)
        if num_errs >= 1:
            error_msg = f"Warning: {num_errs} rule(s) were skipped because they require Pro (try `--pro`), for example: {errors[0]}"
            logger.error(with_color(Colors.red, terminal_wrap(error_msg)))

    def _handle_semgrep_error(self, error: SemgrepError) -> None:
        """
        Reports generic exceptions that extend SemgrepError
        """
        self.has_output = True
        if error not in self.error_set:
            self.semgrep_structured_errors.append(error)
            self.error_set.add(error)
            if self.settings.has_text_output() and (
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

    # group semgrep core errors by path
    @staticmethod
    def _make_failed_to_analyze(
        semgrep_core_errors: Sequence[SemgrepCoreError],
    ) -> Mapping[Path, FileErrorLog]:
        def update_failed_to_analyze(
            memo: Mapping[Path, FileErrorLog],
            err: SemgrepCoreError,
        ) -> Mapping[Path, FileErrorLog]:
            # no associated path
            if not err.core.location:
                return memo
            path = Path(err.core.location.path.value)

            file_error_log = memo.get(path, FileErrorLog())
            file_error_log.add_error(err)
            return {**memo, path: file_error_log}

        return reduce(update_failed_to_analyze, semgrep_core_errors, {})

    def keep_ignores(self) -> bool:
        return bool(
            sum(
                1
                for dest in self.settings.outputs
                if self._formatters[dest].keep_ignores()
            )
        )

    # TODO: why run_scan.scan() calls output() to set the fields why
    # run_scan.run_scan_and_return_json() modify directly the fields instead?
    def output(
        self,
        rule_matches_by_rule: RuleMatchMap,
        *,
        all_targets: Set[Path],
        engine_type: EngineType = EngineType.OSS,
        filtered_rules: List[Rule],
        ignore_log: Optional[FileTargetingLog] = None,
        profiler: Optional[ProfileManager] = None,
        extra: Optional[OutputExtra] = None,
        explanations: Optional[List[out.MatchingExplanation]] = None,
        severities: Optional[Collection[out.MatchSeverity]] = None,
        print_summary: bool = False,
        is_ci_invocation: bool = False,
        executed_rule_count: int = 0,
        missed_rule_count: int = 0,
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
            self.ignore_log = FileTargetingLog(
                TargetManager(
                    scanning_root_strings=frozenset([Path(".")]),
                    use_semgrepignore_v2=True,
                )
            )

        if extra:
            self.extra = extra
        if explanations:
            self.explanations = explanations
        if severities:
            self.severities = severities

        self.is_ci_invocation = is_ci_invocation

        final_error = None
        any_findings_not_ignored = any(
            not rm.match.extra.is_ignored for rm in self.rule_matches
        )

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
            for output_destination, output in self._build_outputs():
                if output_destination:
                    self._save_output(output_destination, output)
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
            blocking_findings = []
            nonblocking_findings = []

            for match in regular_matches:
                if match.is_blocking:
                    blocking_findings.append(match)
                else:
                    nonblocking_findings.append(match)

            num_blocking_findings = len(blocking_findings)
            num_nonblocking_findings = len(nonblocking_findings)

            num_findings = num_blocking_findings + num_nonblocking_findings
            num_targets = len(self.all_targets)
            num_rules = executed_rule_count or len(self.filtered_rules)
            count_line = (
                f"\n • Findings: {num_findings} ({num_blocking_findings} blocking)"
            )
            rule_line = f"\n • Rules run: {num_rules}"
            target_line = f"\n • Targets scanned: {num_targets}"
            ignore_log_str = str(ignore_log) or ""
            ignores_line = ignore_log_str or "\n • No ignore information available"
            suggestion_line = ""
            more_detail_line = ""

            total_lines = sum([line_count_of_path(t) for t in self.all_targets])
            total_lines_skipped = 0
            if self.ignore_log.core_failure_lines_by_file:
                ignore_log = self.ignore_log
                file_error_logs = ignore_log.core_failure_lines_by_file.values()
                total_lines_skipped = sum(
                    [log.num_lines_skipped() or 0 for log in file_error_logs]
                )

            parsed_line = f"\n • Parsed lines: {pretty_print_percentage((total_lines - total_lines_skipped), total_lines)}"

            if ignore_log_str or total_lines_skipped:
                more_detail_line = "\n • For a detailed list of skipped files and lines, run semgrep with the --verbose flag"
            if (
                num_findings == 0
                and num_targets > 0
                and num_rules > 0
                and state.metrics.is_using_registry
                and (not auth.is_logged_in_weak())
            ):
                suggestion_line = "\n(need more rules? `semgrep login` for additional free Semgrep Registry rules)\n"
            stats_line = ""
            if print_summary:
                stats_line = f"\nRan {unit_str(num_rules, 'rule')} on {unit_str(num_targets, 'file')}: {unit_str(num_findings, 'finding')}."
                if (
                    missed_rule_count
                    and state.get_cli_ux_flavor() != DesignTreatment.LEGACY
                ):
                    missed_count_line = f"💎 Missed out on {unit_str(missed_rule_count, 'pro rule')} since you aren't logged in!"
                    learn_more_url = with_color(
                        Colors.cyan, "https://sg.run/rules", underline=True
                    )
                    learn_more_line = f"⚡ Supercharge Semgrep OSS when you create a free account at {learn_more_url}."
                    stats_line = f"{stats_line}\n{missed_count_line}\n{learn_more_line}"
            if ignore_log is not None:
                too_many_entries = self.settings.max_log_list_entries
                logger.verbose(ignore_log.verbose_output(too_many_entries))

            success_line = "✅ " + (
                "CI scan completed successfully."
                if self.is_ci_invocation
                else "Scan completed successfully."
            )
            output_text = (
                success_line
                + count_line
                + rule_line
                + target_line
                + parsed_line
                + ignores_line
                + more_detail_line
                + stats_line
                + suggestion_line
            )
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

    def _build_outputs(self) -> Iterator[Tuple[Optional[str], str]]:
        for output_destination, output_format in self.settings.get_outputs():
            yield self._build_output(output_destination, output_format)

    def _build_output(
        self, output_destination: Optional[str], output_format: OutputFormat
    ) -> Tuple[Optional[str], str]:
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

        if self.settings.output_time and self.extra and self.extra.core.time:
            cli_timing = _build_time_json(
                self.filtered_rules,
                self.all_targets,
                self.extra.core.time,
                self.profiler,
            )

        # DO NOT USE THIS local!
        # The extra dict is for blatantly skipping type checking and function signatures.
        # - The text formatter uses it to store settings
        # You should use CliOutputExtra for better type checking
        extra: Dict[str, Any] = {}

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
        if output_format == OutputFormat.TEXT:
            extra["color_output"] = (
                (output_destination is None and sys.stdout.isatty())
                or os.environ.get("SEMGREP_FORCE_COLOR")
            ) and not os.environ.get("NO_COLOR")
            extra[
                "per_finding_max_lines_limit"
            ] = self.settings.output_per_finding_max_lines_limit
            extra[
                "per_line_max_chars_limit"
            ] = self.settings.output_per_line_max_chars_limit
            extra["dataflow_traces"] = self.settings.dataflow_traces
        if output_format == OutputFormat.SARIF:
            extra["dataflow_traces"] = self.settings.dataflow_traces

        # as opposed to below, we need to distinguish the various kinds of pro engine
        extra["engine_requested"] = self.engine_type

        # TODO: I thought we could guard this code with 'if self.extra:', and raise
        # a SemgrepError otherwise, but it seems that when semgrep got an error
        # (for example in tests/default/e2e/test_ci.py::test_bad_config),
        # then this code still get called and self.extra is not set but we still want
        # to output things. This is why I have those ugly 'if self.extra' below
        # that possibly return None.

        cli_output_extra = out.CliOutputExtra(
            # TODO: almost like self.extra.core.paths, but not there yet
            paths=cli_paths,
            # TODO: almost like self.extra.core.time, but not there yet
            time=cli_timing,
            # TODO: would like t ouse self.extra.core.explanations byt regressions
            explanations=explanations,
            rules_by_engine=self.extra.core.rules_by_engine if self.extra else None,
            # this flattens the information into just distinguishing "pro" and "not-pro"
            engine_requested=self.engine_type.to_engine_kind(),
            interfile_languages_used=(
                self.extra.core.interfile_languages_used if self.extra else None
            ),
            # TODO, should just be self.extra.core.skipped_rules
            skipped_rules=[],
        )

        state = get_state()
        formatter = self._formatters[output_destination]
        output = formatter.output(  # the rules are used only by the SARIF formatter
            self.rules,
            self.rule_matches,
            self.semgrep_structured_errors,
            cli_output_extra,
            extra,
            self.severities,
            out.FormatContext(
                is_ci_invocation=self.is_ci_invocation,
                is_logged_in=auth.is_logged_in_weak(),
                # If users are not using our registry, we will not nudge them to login
                is_using_registry=state.metrics.is_using_registry
                or state.env.mock_using_registry,
            ),
        )
        return (output_destination, output)
