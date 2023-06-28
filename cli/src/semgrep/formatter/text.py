import textwrap
from contextlib import contextmanager
from itertools import groupby
from pathlib import Path
from shutil import get_terminal_size
from typing import Any
from typing import cast
from typing import Dict
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Tuple

import click
import colorama
from rich.console import Console

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.console import console
from semgrep.console import Title
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import Colors
from semgrep.constants import ELLIPSIS_STRING
from semgrep.constants import MAX_CHARS_FLAG_NAME
from semgrep.constants import MAX_LINES_FLAG_NAME
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule import RuleProduct
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.util import format_bytes
from semgrep.util import get_lines
from semgrep.util import truncate
from semgrep.util import unit_str
from semgrep.util import with_color

MAX_TEXT_WIDTH = 120

BASE_INDENT = 8

terminal_size = get_terminal_size((MAX_TEXT_WIDTH, 1))[0]
if terminal_size <= 0:
    terminal_size = MAX_TEXT_WIDTH
width = min(MAX_TEXT_WIDTH, terminal_size)
if width <= 110:
    width = width - 5
else:
    width = width - (width - 100)

FINDINGS_INDENT_DEPTH = 10


GROUP_TITLES: Dict[Tuple[RuleProduct, str], str] = {
    (RuleProduct.sca, "unreachable"): "Unreachable Supply Chain Finding",
    (RuleProduct.sca, "undetermined"): "Undetermined Supply Chain Finding",
    (RuleProduct.sca, "reachable"): "Reachable Supply Chain Finding",
    (RuleProduct.sast, "nonblocking"): "Non-blocking Code Finding",
    (RuleProduct.sast, "blocking"): "Blocking Code Finding",
    (RuleProduct.sast, "merged"): "Code Finding",
}


def color_line(
    line: str,
    line_number: int,
    start_line: int,
    start_col: int,
    end_line: int,
    end_col: int,
) -> str:
    """
    Assumes column start and end numbers are 1-indexed
    """
    start_color = 0 if line_number > start_line else start_col
    # adjust for 1-indexed column number
    start_color = max(start_color - 1, 0)
    # put the end color at the end of the line if this isn't the last line in the output
    end_color = end_col if line_number >= end_line else len(line) + 1 + 1
    # adjust for 1-indexed column number
    end_color = max(end_color - 1, 0)
    line = (
        line[:start_color]
        + with_color(Colors.foreground, line[start_color:end_color], bold=True)
        + line[end_color:]
    )
    return line


def format_lines(
    path: Path,
    start_line: int,
    start_col: int,
    end_line: int,
    end_col: int,
    lines: List[str],
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    show_separator: bool,
    show_path: bool,
) -> Iterator[str]:
    trimmed = 0
    stripped = False

    if per_finding_max_lines_limit:
        trimmed = len(lines) - per_finding_max_lines_limit
        lines = lines[:per_finding_max_lines_limit]

    # we remove indentation at the start of the snippet to avoid wasting space
    dedented_lines = textwrap.dedent("".join(lines)).splitlines()
    indent_len = (
        len(lines[0].rstrip()) - len(dedented_lines[0].rstrip())
        if len(dedented_lines) > 0 and len(lines) > 0
        else 0
    )

    # since we dedented each line, we need to adjust where the highlighting is
    start_col -= indent_len
    end_col -= indent_len

    for i, line in enumerate(dedented_lines):
        line = line.rstrip()
        line_number = ""
        if start_line:
            if color_output:
                line = color_line(
                    line,
                    start_line + i,
                    start_line,
                    start_col,
                    end_line,
                    end_col,
                )
                line_number = f"{start_line + i}"
            else:
                line_number = f"{start_line + i}"

            if per_line_max_chars_limit and len(line) > per_line_max_chars_limit:
                stripped = True
                is_first_line = i == 0
                if is_first_line:
                    line = (
                        line[start_col - 1 : start_col - 1 + per_line_max_chars_limit]
                        + ELLIPSIS_STRING
                    )
                    if start_col > 1:
                        line = ELLIPSIS_STRING + line
                else:
                    line = line[:per_line_max_chars_limit] + ELLIPSIS_STRING
                # while stripping a string, the ANSI code for resetting color might also get stripped.
                line = line + colorama.Style.RESET_ALL

        # plus one because we want this to be slightly separated from the intervening messages
        if i == 0 and show_path:
            yield f" " * (
                BASE_INDENT + 1
            ) + f"{with_color(Colors.cyan, f'{path}', bold=False)}"

        yield f" " * (
            11 - len(line_number)
        ) + f"{line_number}┆ {line}" if line_number else f"{line}"

    if stripped:
        stripped_str = (
            f"[shortened a long line from output, adjust with {MAX_CHARS_FLAG_NAME}]"
        )
        yield " " * FINDINGS_INDENT_DEPTH + stripped_str

    if per_finding_max_lines_limit != 1:
        if trimmed > 0:
            trimmed_str = (
                f" [hid {trimmed} additional lines, adjust with {MAX_LINES_FLAG_NAME}] "
            )
            yield " " * FINDINGS_INDENT_DEPTH + trimmed_str
        elif lines and show_separator:
            yield f" " * FINDINGS_INDENT_DEPTH + f"⋮┆" + f"-" * 40


def finding_to_line(
    rule_match: RuleMatch,
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    show_separator: bool,
) -> Iterator[str]:
    path = rule_match.path
    start_line = rule_match.start.line
    end_line = rule_match.end.line
    start_col = rule_match.start.col
    end_col = rule_match.end.col
    if path:
        lines = rule_match.extra.get("fixed_lines") or rule_match.lines
        yield from format_lines(
            path,
            start_line,
            start_col,
            end_line,
            end_col,
            lines,
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
            show_separator,
            False,
        )


def match_to_lines(
    ref_path: Path,
    location: out.Location,
    content: str,
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
) -> Iterator[str]:
    path = Path(location.path.value)
    is_same_file = path == ref_path
    lines = get_lines(path, location.start.line, location.end.line)
    yield from format_lines(
        path,
        location.start.line,
        location.start.col,
        location.end.line,
        location.end.col,
        lines,
        color_output,
        per_finding_max_lines_limit,
        per_line_max_chars_limit,
        False,
        not is_same_file,
    )


def call_trace_to_lines(
    ref_path: Path,
    call_trace: out.CliMatchCallTrace,
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
) -> Iterator[str]:
    trace = call_trace.value
    if isinstance(trace, out.CliLoc):
        yield from match_to_lines(
            ref_path,
            trace.value[0],
            trace.value[1],
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
        )

    elif isinstance(trace, out.CliCall):
        data, intermediate_vars, call_trace = trace.value

        yield from match_to_lines(
            ref_path,
            data[0],
            data[1],
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
        )

        if intermediate_vars and len(intermediate_vars) > 0:
            # TODO change this message based on rule kind if we ever use
            # dataflow traces for more than just taint
            yield (
                BASE_INDENT * " " + "Taint flows through these intermediate variables:"
            )
            prev_path = ref_path
            for var in intermediate_vars:
                loc = var.location
                path = Path(loc.path.value)
                lines = get_lines(Path(loc.path.value), loc.start.line, loc.end.line)
                is_same_file = path == prev_path
                yield from format_lines(
                    Path(loc.path.value),
                    loc.start.line,
                    loc.start.col,
                    loc.end.line,
                    loc.end.col,
                    lines,
                    color_output,
                    per_finding_max_lines_limit,
                    per_line_max_chars_limit,
                    False,
                    not is_same_file,
                )
                prev_path = path

        if isinstance(call_trace.value, out.CliCall):
            yield (BASE_INDENT * " " + "then call to:")
        elif isinstance(call_trace.value, out.CliLoc):
            yield (BASE_INDENT * " " + "then reaches:")
        yield from call_trace_to_lines(
            ref_path,
            call_trace,
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
        )


def dataflow_trace_to_lines(
    rule_match_path: Path,
    dataflow_trace: Optional[out.CliMatchDataflowTrace],
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    show_separator: bool,
) -> Iterator[str]:
    if dataflow_trace:
        source = dataflow_trace.taint_source
        intermediate_vars = dataflow_trace.intermediate_vars
        sink = dataflow_trace.taint_sink

        if source:
            yield ""
            yield (BASE_INDENT * " " + "Taint comes from:")
            yield from call_trace_to_lines(
                rule_match_path,
                source,
                color_output,
                per_finding_max_lines_limit,
                per_line_max_chars_limit,
            )

        if intermediate_vars and len(intermediate_vars) > 0:
            # TODO change this message based on rule kind of we ever use
            # dataflow traces for more than just taint
            yield ""
            yield (
                BASE_INDENT * " " + "Taint flows through these intermediate variables:"
            )
            prev_path = rule_match_path
            for var in intermediate_vars:
                loc = var.location
                path = Path(loc.path.value)
                lines = get_lines(path, loc.start.line, loc.end.line)
                is_same_file = path == prev_path
                yield from format_lines(
                    path,
                    loc.start.line,
                    loc.start.col,
                    loc.end.line,
                    loc.end.col,
                    lines,
                    color_output,
                    per_finding_max_lines_limit,
                    per_line_max_chars_limit,
                    False,
                    not is_same_file,
                )
                prev_path = path

        if sink:
            yield ""
            yield (BASE_INDENT * " " + "This is how taint reaches the sink:")
            yield from call_trace_to_lines(
                rule_match_path,
                sink,
                color_output,
                per_finding_max_lines_limit,
                per_line_max_chars_limit,
            )
            yield ""

        if source and show_separator:
            yield f" " * BASE_INDENT + f"⋮┆" + f"-" * 40


def get_details_shortlink(rule_match: RuleMatch) -> Optional[str]:
    source_url = rule_match.metadata.get("shortlink")
    if not source_url:
        return ""
    return f"Details: {source_url}"


def print_time_summary(
    time_data: out.CliTiming, error_output: Sequence[SemgrepError]
) -> None:
    items_to_show = 5
    col_lim = 50

    targets = time_data.targets

    # Compute summary timings
    rule_parsing_time = time_data.rules_parse_time
    rule_match_timings = {
        rule.id.value: sum(t.match_times[i] for t in targets if t.match_times[i] >= 0)
        for i, rule in enumerate(time_data.rules)
    }
    file_parsing_time = sum(
        sum(t for t in target.parse_times if t >= 0) for target in targets
    )
    file_timings = {
        target.path.value: (
            sum(t for t in target.parse_times if t >= 0),
            target.run_time,
        )
        for target in targets
    }

    all_total_time = sum(i[1] for i in file_timings.values()) + rule_parsing_time
    total_matching_time = sum(i for i in rule_match_timings.values())

    # Count errors

    semgrep_core_errors = [
        cast(SemgrepCoreError, err)
        for err in error_output
        if SemgrepError.semgrep_error_type(err) == "SemgrepCoreError"
    ]
    errors = {
        (err.core.location.path.value, err.core.error_type.kind)
        for err in semgrep_core_errors
    }

    error_types = {k: len(list(v)) for k, v in groupby(errors, lambda x: x[1])}
    num_errors = len(errors)

    # Compute summary by language

    # TODO assumes languages correspond solely to extension
    # Consider: get a report from semgrep-core on what language each
    #           file was analyzed as
    # However, this might make it harder for users to confirm
    # semgrep counts against their expected file counts

    ext_to_lang: Mapping[str, Language] = LANGUAGE.lang_by_ext

    def lang_of_path(path: str) -> str:
        ext = "." + path.split(".")[-1]
        return ext_to_lang.get(ext, "generic")

    ext_info = sorted(
        (
            (
                lang_of_path(target.path.value),
                (target.num_bytes, target.run_time),
            )
            for target in targets
        ),
        key=lambda x: x[0],
    )
    lang_info = {k: list(v) for k, v in groupby(ext_info, lambda x: x[0])}
    langs = lang_info.keys()
    lang_counts: Mapping[str, int] = {lang: len(lang_info[lang]) for lang in langs}
    lang_bytes: Mapping[str, int] = {
        lang: sum(info[1][0] for info in lang_info[lang]) for lang in langs
    }
    lang_times: Mapping[str, float] = {
        lang: sum(info[1][1] for info in lang_info[lang]) for lang in langs
    }

    # Output semgrep summary
    total_time = time_data.profiling_times.get("total_time", 0.0)
    config_time = time_data.profiling_times.get("config_time", 0.0)
    core_time = time_data.profiling_times.get("core_time", 0.0)
    # time_data.profiling_times.get("ignores_time", 0.0)

    console.print(
        "\n============================[ summary ]============================"
    )

    console.print(
        f"Total time: {total_time:.4f}s Config time: {config_time:.4f}s Core time: {core_time:.4f}s"
    )

    # Output semgrep-core information
    console.print("\nSemgrep-core time:")
    console.print(
        f"Total CPU time: {all_total_time:.4f}s  File parse time: {file_parsing_time:.4f}s"
        f"  Rule parse time: {rule_parsing_time:.4f}s  Match time: {total_matching_time:.4f}s"
    )

    console.print(f"Slowest {items_to_show}/{len(file_timings)} files")
    slowest_file_times = sorted(file_timings.items(), key=lambda x: x[1], reverse=True)[
        :items_to_show
    ]
    for file_name, (parse_time, run_time) in slowest_file_times:
        num_bytes = f"({format_bytes(Path(file_name).resolve().stat().st_size)}):"
        file_name = truncate(file_name, col_lim)
        console.print(
            f"{with_color(Colors.green, f'{file_name:<50}')} {num_bytes:<8} {run_time:.3f}s ({parse_time:.3f}s to parse)"
        )

    console.print(f"Slowest {items_to_show} rules to match")
    slowest_rule_times = sorted(rule_match_timings.items(), reverse=True)[
        :items_to_show
    ]
    for rule_id, match_time in slowest_rule_times:
        rule_id = truncate(rule_id, col_lim) + ":"
        console.print(
            f"{with_color(Colors.yellow, f'{rule_id:<59}')} {match_time:.3f}s"
        )

    # Output other file information
    ANALYZED = "Analyzed:"
    FAILED = "Errors:"
    headings = [ANALYZED, FAILED]
    max_heading_len = max(len(h) for h in headings) + 1  # for the space

    def add_heading(heading: str, lines: List[str]) -> List[str]:
        heading = heading + " " * (max_heading_len - len(heading))
        first = True
        returned = []
        for line in lines:
            prefix = heading if first else " " * max_heading_len
            returned.append(f"{prefix}{line}")
            first = False
        return returned

    console.print()

    by_lang = [
        f"{ lang_counts[lang] } { lang } files ({ format_bytes(lang_bytes[lang]) } in {(lang_times[lang]):.3f} seconds)"
        for lang in langs
    ]
    console.print("\n".join(add_heading(ANALYZED, by_lang)))

    # Output errors
    def if_exists(num_errors: int, msg: str) -> str:
        return "" if num_errors == 0 else msg

    see_more = if_exists(
        num_errors,
        ", see output before the results for details or run with --strict",
    )
    error_msg = f"{ num_errors } files with errors{see_more}"
    error_lines = [error_msg] + [
        f"{type} ({num} files)" for (type, num) in error_types.items()
    ]
    console.print("\n".join(add_heading(FAILED, error_lines)))
    console.print()


def print_text_output(
    rule_matches: Iterable[RuleMatch],
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    dataflow_traces: bool,
) -> None:
    last_file = None
    last_message = None
    sorted_rule_matches = sorted(rule_matches, key=lambda r: (r.path, r.rule_id))
    for rule_index, rule_match in enumerate(sorted_rule_matches):
        current_file = rule_match.path
        message = rule_match.message
        fix = rule_match.fix
        if "sca_info" in rule_match.extra and (rule_match.extra["sca_info"].reachable):
            lockfile = rule_match.extra["sca_info"].dependency_match.lockfile
        else:
            lockfile = None
        if last_file is None or last_file != current_file:
            if last_file is not None:
                console.print()
            console.print(
                f"\n{with_color(Colors.cyan, f'  {current_file} ', bold=False)}"
                + (
                    f"with lockfile {with_color(Colors.cyan, f'{lockfile}')}"
                    if lockfile
                    else ""
                )
            )
            last_message = None
        # don't display the rule line if the check is empty
        if (
            rule_match.rule_id
            and rule_match.rule_id != CLI_RULE_ID
            and (last_message is None or last_message != message)
        ):
            shortlink = get_details_shortlink(rule_match)
            shortlink_text = (8 * " " + shortlink + "\n") if shortlink else ""
            title_text = click.wrap_text(
                f"{with_color(Colors.foreground, rule_match.title, bold=True)}",
                width + 10,
                5 * " ",
                5 * " ",
                False,
            )
            severity = (
                (
                    f"{8 * ' '}Severity: {with_color(Colors.foreground, rule_match.metadata['sca-severity'], bold=True)}\n"
                )
                if "sca_info" in rule_match.extra
                and "sca-severity" in rule_match.metadata
                else ""
            )
            message_text = click.wrap_text(f"{message}", width, 8 * " ", 8 * " ", True)
            console.print(f"{title_text}\n{severity}{message_text}\n{shortlink_text}")

        autofix_tag = with_color(Colors.green, "         ▶▶┆ Autofix ▶")
        if fix is not None:
            console.print(
                f"{autofix_tag} {fix if fix else with_color(Colors.red, 'delete')}"
            )
        elif rule_match.fix_regex:
            fix_regex = rule_match.fix_regex
            console.print(
                f"{autofix_tag} s/{fix_regex.regex}/{fix_regex.replacement}/{fix_regex.count or 'g'}"
            )
        elif (
            "sca_info" in rule_match.extra
            and "sca-fix-versions" in rule_match.metadata
            and (last_message is None or last_message != message)
        ):
            # this is a list of objects like [{'minimist': '0.2.4'}, {'minimist': '1.2.6'}]
            fixes = rule_match.metadata["sca-fix-versions"]
            # will be structure { 'package_name': set('1.2.3', '2.3.4') }
            dep_name = rule_match.extra[
                "sca_info"
            ].dependency_match.found_dependency.package
            fixed_versions = sorted(
                {
                    version
                    for fix_obj in fixes
                    for name, version in fix_obj.items()
                    if name == dep_name
                }
            )
            version_txt = "versions" if len(fixed_versions) > 1 else "version"
            console.print(
                with_color(
                    Colors.green,
                    f"         ▶▶┆ Fixed for {dep_name} at {version_txt}: {', '.join(fixed_versions)}",
                )
            )

        last_file = current_file
        last_message = message
        next_rule_match = (
            sorted_rule_matches[rule_index + 1]
            if rule_index != len(sorted_rule_matches) - 1
            else None
        )
        is_same_file = (
            next_rule_match.path == rule_match.path if next_rule_match else False
        )
        for line in finding_to_line(
            rule_match,
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
            # if we have dataflow traces on, then we should print the separator,
            # because otherwise it is easy to mistake taint traces as belonging
            # to a different finding
            is_same_file and not (dataflow_traces and rule_match.dataflow_trace),
        ):
            console.print(line)

        if dataflow_traces:
            for line in dataflow_trace_to_lines(
                rule_match.path,
                rule_match.dataflow_trace,
                color_output,
                per_finding_max_lines_limit,
                per_line_max_chars_limit,
                is_same_file,
            ):
                console.print("  " + line)


@contextmanager
def force_quiet_off(console: Console) -> Iterator[None]:
    """
    Force the console to be not quiet, even if it was set to be quiet before.
    """
    was_quiet = console.quiet
    console.quiet = False
    try:
        yield
    finally:
        console.quiet = was_quiet


class TextFormatter(BaseFormatter):
    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        is_ci_invocation: bool,
    ) -> str:
        # all output in this function is captured and returned as a string
        with force_quiet_off(console), console.capture() as captured_output:
            grouped_matches: Dict[Tuple[RuleProduct, str], List[RuleMatch]] = {
                # ordered most important to least important
                (RuleProduct.sast, "blocking"): [],
                (RuleProduct.sca, "reachable"): [],
                (RuleProduct.sca, "undetermined"): [],
                (RuleProduct.sca, "unreachable"): [],
                (RuleProduct.sast, "nonblocking"): [],
            }

            for match in rule_matches:
                if match.product == RuleProduct.sast:
                    subgroup = "blocking" if match.is_blocking else "nonblocking"
                else:
                    subgroup = match.exposure_type or "undetermined"

                grouped_matches[match.product, subgroup].append(match)

            first_party_blocking_rules = {
                match.match.rule_id.value
                for match in grouped_matches[RuleProduct.sast, "blocking"]
            }

            # When ephemeral rules are run with the -e or --pattern flag in the command-line, the rule_id is set to -.
            # The rule is ran in the command-line and has no associated rule_id
            first_party_blocking_rules.discard("-")

            if not is_ci_invocation:
                grouped_matches[(RuleProduct.sast, "merged")] = [
                    *grouped_matches.pop((RuleProduct.sast, "nonblocking")),
                    *grouped_matches.pop((RuleProduct.sast, "blocking")),
                ]

            for group, matches in grouped_matches.items():
                if not matches:
                    continue
                console.print(Title(unit_str(len(matches), GROUP_TITLES[group])))
                print_text_output(
                    matches,
                    extra.get("color_output", False),
                    extra["per_finding_max_lines_limit"],
                    extra["per_line_max_chars_limit"],
                    extra["dataflow_traces"],
                )

            if first_party_blocking_rules and is_ci_invocation:
                console.print(Title("Blocking Code Rules Fired:", order=2))
                for rule_id in sorted(first_party_blocking_rules):
                    console.print(f"  {rule_id}")
                console.reset_title(order=1)

            if cli_output_extra.time:
                print_time_summary(cli_output_extra.time, semgrep_structured_errors)

            rules_by_engine = (
                cli_output_extra.rules_by_engine
                if cli_output_extra.rules_by_engine
                else []
            )

            rules_ran_within_a_file = [
                with_color(Colors.foreground, rule.value[0].value, bold=True)
                for rule in rules_by_engine
                if isinstance(rule.value[1].value, out.OSS)
            ]

            if (extra["engine_requested"].is_interfile) and rules_ran_within_a_file:
                console.print(
                    f"{unit_str(len(rules_ran_within_a_file), 'rule')} ran in a within-a-file fashion"
                    + " because `interfile: true` was not specified."
                )
                if extra.get("verbose_errors"):
                    console.print(
                        "These rules were:\n   "
                        + "   \n   ".join(rules_ran_within_a_file)
                    )
                else:
                    console.print("(Use --verbose to see which ones.)")

        return captured_output.get()
