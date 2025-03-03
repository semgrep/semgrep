import textwrap
from contextlib import contextmanager
from itertools import groupby
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence
from typing import Tuple

import click
from rich.console import Console
from rich.text import Text

import semgrep.formatter.base as base
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.console import console
from semgrep.console import Title
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import Colors
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.state import get_state
from semgrep.util import format_bytes
from semgrep.util import get_lines_from_file
from semgrep.util import MASK_CHAR
from semgrep.util import MASK_SHOW_PCT
from semgrep.util import truncate
from semgrep.util import unit_str
from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

RULE_INDENT = 5  # NOTE: There are 2 leading spaces not included in this number
BASE_INDENT = 8
FINDINGS_INDENT_DEPTH = 14
BASE_WIDTH = console.width
# NOTE: All text widths must be > 0 to avoid runtime value errors.
#       As a preventative measure, we set a minimum width of 10
#       for all text widths, with the wrapper function `safe_width`.
MIN_TEXT_WIDTH = 10
FINDINGS_TEXT_WIDTH = BASE_WIDTH - (FINDINGS_INDENT_DEPTH + 2)
RULE_TEXT_WIDTH = BASE_WIDTH - (RULE_INDENT + 4)
DESC_TEXT_WIDTH = BASE_WIDTH - (BASE_INDENT + 4)
AUTOFIX_TEXT_WIDTH = BASE_WIDTH - (
    BASE_INDENT + 4 + 13
)  # 4 for line number, 13 for autofix tag


# coupling: some of the string below must match rule_match.exposure_type()
# TODO: use Enum?
GROUP_TITLES: Dict[Tuple[out.Product, str], str] = {
    (out.Product(out.SCA()), "unreachable"): "Unreachable Supply Chain Finding",
    (out.Product(out.SCA()), "undetermined"): "Undetermined Supply Chain Finding",
    (out.Product(out.SCA()), "reachable"): "Reachable Supply Chain Finding",
    (out.Product(out.SAST()), "nonblocking"): "Non-blocking Code Finding",
    (out.Product(out.SAST()), "blocking"): "Blocking Code Finding",
    (out.Product(out.SAST()), "merged"): "Code Finding",
    (out.Product(out.Secrets()), "valid"): "Valid Secrets Finding",
    (out.Product(out.Secrets()), "invalid"): "Invalid Secrets Finding",
    (out.Product(out.Secrets()), "unvalidated"): "Unvalidated Secrets Finding",
    (out.Product(out.Secrets()), "generic-secrets"): "Generic Secrets Finding",
    (
        out.Product(out.Secrets()),
        "validation error",
    ): "Secrets Validation Error",
}

SEVERITY_MAP_PLAIN = {
    out.Critical.to_json(): ("", "❯❯❯❱"),
    out.Error.to_json(): ("", "❯❯❱"),
    out.High.to_json(): ("", "❯❯❱"),
    out.Warning.to_json(): ("", " ❯❱"),
    out.Medium.to_json(): ("", " ❯❱"),
    out.Info.to_json(): ("", "  ❱"),
    out.Low.to_json(): ("", "  ❱"),
}

SEVERITY_MAP_STYLED = {
    out.Critical.to_json(): ("magenta", "❯❯❯❱"),
    out.Error.to_json(): ("red", "❯❯❱"),
    out.High.to_json(): ("red", "❯❯❱"),
    out.Warning.to_json(): ("yellow", " ❯❱"),
    out.Medium.to_json(): ("yellow", " ❯❱"),
    out.Info.to_json(): ("green", "  ❱"),
    out.Low.to_json(): ("green", "  ❱"),
}


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


def safe_width(width: int) -> int:
    """
    Return a width that is at least MIN_TEXT_WIDTH to avoid
    runtime value errors.
    """
    return max(MIN_TEXT_WIDTH, width)


def to_severity_indicator(
    rule_match: RuleMatch,
    color_output: bool = False,
) -> Tuple[str, str]:
    """Return a color and severity icon."""
    severity = rule_match.severity.to_json()
    lookup = SEVERITY_MAP_STYLED if color_output else SEVERITY_MAP_PLAIN
    return lookup.get(severity, ("", "   "))


def format_finding_line(
    line: str,
    line_number: int,
    start_line: int,
    start_col: int,
    end_line: int,
    end_col: int,
    color: bool,
    mask: bool,
    per_line_max_chars_limit: int,
) -> Text:
    """
    Assumes column start and end numbers are 1-indexed
    """
    start = 0 if line_number > start_line else start_col
    # adjust for 1-indexed column number
    start = max(start - 1, 0)
    # put the end color at the end of the line if this isn't the last line in the output
    end = end_col if line_number >= end_line else len(line) + 1 + 1
    # adjust for 1-indexed column number
    end = max(end - 1, 0)
    if mask:
        show_until = int(MASK_SHOW_PCT * (end - start)) + start
        mid = line[start:show_until] + (MASK_CHAR * (end - show_until))
    else:
        mid = line[start:end]
    # adjust for 1-indexed line number and add separator
    line_number_str = f"{line_number}┆ ".rjust(5)  # 3 digits + 1 separator + 1 space
    # use a marker to indicate where we have the match for colored replacement
    mid_styled = mid or "" if not color else f"▶{mid}◀"
    wrapped_text = textwrap.fill(
        f"{line_number_str}{line[:start]}{mid_styled}{line[end:]}",
        width=safe_width(per_line_max_chars_limit),
        initial_indent=(FINDINGS_INDENT_DEPTH - 6) * " ",
        subsequent_indent=(FINDINGS_INDENT_DEPTH - 1) * " ",
    )
    # manually apply bolding when color is enabled to avoid wrapping issues
    wrapped_text = wrapped_text.replace("▶", "\033[1m").replace("◀", "\033[0m")
    return Text.from_ansi(wrapped_text)


def format_lines(
    path: Path,
    start_line: int,
    start_col: int,
    end_line: int,
    end_col: int,
    lines: List[str],
    color_output: bool,
    mask_match: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    show_separator: bool,
    is_different_file: bool,
) -> Iterator[Text]:
    trimmed = 0

    if per_finding_max_lines_limit:
        trimmed = len(lines) - per_finding_max_lines_limit
        lines = lines[:per_finding_max_lines_limit]

    per_line_max_chars_limit = min(
        per_line_max_chars_limit or FINDINGS_TEXT_WIDTH,
        FINDINGS_TEXT_WIDTH,
    )
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
        # Taint findings can span multiple files, so the line number reported by
        # 'format_finding_line' below may not belong to the same file where the
        # finding is reported! For the very first line, if 'is_different_file'
        # is True, we print the file.
        if i == 0 and is_different_file:
            yield Text.from_ansi(
                f" " * (BASE_INDENT + 1)
                + f"{with_color(Colors.cyan, f'{path}', bold=False)}"
            )
        # NOTE: need to consider length of line number when calculating max chars
        yield format_finding_line(
            line,
            start_line + i,
            start_line,
            start_col,
            end_line,
            end_col,
            color=color_output,
            mask=mask_match,
            per_line_max_chars_limit=per_line_max_chars_limit,
        )

    if per_finding_max_lines_limit == 1:
        return

    if trimmed > 0:
        yield Text.assemble(
            " " * (FINDINGS_INDENT_DEPTH - 4),
            f" [hid {trimmed} additional lines, adjust with --max-lines-per-finding] ",
        )
    elif lines and show_separator:
        longest_line_len = min(
            per_line_max_chars_limit, max(len(line) for line in lines)
        )
        seperator_fill_count = longest_line_len - 1
        # TODO: re-enable dynamic size in a separate PR to avoid too many test changes
        seperator_fill_count = 40 if 1 else seperator_fill_count  # dummy if statement
        yield Text.assemble(
            f" " * (FINDINGS_INDENT_DEPTH - 4) + f"⋮┆" + f"-" * seperator_fill_count
        )


def finding_to_line(
    rule_match: RuleMatch,
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    show_separator: bool,
) -> Iterator[Text]:
    path = rule_match.path
    start_line = rule_match.start.line
    end_line = rule_match.end.line
    start_col = rule_match.start.col
    end_col = rule_match.end.col
    if path:
        lines = rule_match._fixed_lines or rule_match.lines
        yield from format_lines(
            path,
            start_line,
            start_col,
            end_line,
            end_col,
            lines,
            color_output,
            isinstance(rule_match.product.value, out.Secrets),
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
) -> Iterator[Text]:
    path = Path(location.path.value)
    # If 'is_different_file' is True, it means that we are going to print a
    # bunch of lines that belong to a different file, so instruct 'format_lines'
    # to print the name of that file too.
    is_different_file = path != ref_path
    try:
        lines = get_lines_from_file(path, location.start.line, location.end.line)
    except FileNotFoundError:
        # Use ‘content’ instead when the file at the specified location doesn’t exist.
        # This can happen if the taint trace shows a match in a temporary fake lifecycle
        # file used for web framework analysis. The issue is specific to pysemgrep, as
        # for osemgrep, the temporary file isn’t deleted until osemgrep finishes.
        lines = [content]
    yield from format_lines(
        path,
        location.start.line,
        location.start.col,
        location.end.line,
        location.end.col,
        lines,
        color_output,
        False,
        per_finding_max_lines_limit,
        per_line_max_chars_limit,
        False,
        is_different_file,
    )


def call_trace_to_lines(
    ref_path: Path,
    call_trace: out.MatchCallTrace,
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
) -> Iterator[Text]:
    trace = call_trace.value
    if isinstance(trace, out.CliLoc):
        yield from match_to_lines(
            ref_path,
            trace.value.value[0],
            trace.value.value[1],
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
        )

    elif isinstance(trace, out.CliCall):
        data, intermediate_vars, call_trace = trace.value

        yield from match_to_lines(
            ref_path,
            data.value[0],
            data.value[1],
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
        )

        if intermediate_vars and len(intermediate_vars) > 0:
            # TODO change this message based on rule kind if we ever use
            # dataflow traces for more than just taint
            yield Text.assemble(
                BASE_INDENT * " " + "Taint flows through these intermediate variables:"
            )
            prev_path = ref_path
            for var in intermediate_vars:
                loc = var.location
                path = Path(loc.path.value)
                try:
                    lines = get_lines_from_file(
                        Path(loc.path.value), loc.start.line, loc.end.line
                    )
                except FileNotFoundError:
                    lines = [var.content]
                is_different_file = path != prev_path
                yield from format_lines(
                    Path(loc.path.value),
                    loc.start.line,
                    loc.start.col,
                    loc.end.line,
                    loc.end.col,
                    lines,
                    color_output,
                    False,
                    per_finding_max_lines_limit,
                    per_line_max_chars_limit,
                    False,
                    is_different_file,
                )
                prev_path = path

        if isinstance(call_trace.value, out.CliCall):
            yield Text.assemble(BASE_INDENT * " " + "then call to:")
        elif isinstance(call_trace.value, out.CliLoc):
            yield Text.assemble(BASE_INDENT * " " + "then reaches:")
        yield from call_trace_to_lines(
            ref_path,
            call_trace,
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
        )


def dataflow_trace_to_lines(
    rule_match_path: Path,
    dataflow_trace: Optional[out.MatchDataflowTrace],
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    show_separator: bool,
) -> Iterator[Text]:
    if dataflow_trace:
        source = dataflow_trace.taint_source
        intermediate_vars = dataflow_trace.intermediate_vars
        sink = dataflow_trace.taint_sink

        if source:
            yield Text.assemble("")
            yield Text.assemble(BASE_INDENT * " " + "Taint comes from:")
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
            yield Text.assemble("")
            yield Text.assemble(
                BASE_INDENT * " " + "Taint flows through these intermediate variables:"
            )
            prev_path = rule_match_path
            for var in intermediate_vars:
                loc = var.location
                path = Path(loc.path.value)
                try:
                    lines = get_lines_from_file(path, loc.start.line, loc.end.line)
                except FileNotFoundError:
                    lines = [var.content]
                is_different_file = path != prev_path
                yield from format_lines(
                    path,
                    loc.start.line,
                    loc.start.col,
                    loc.end.line,
                    loc.end.col,
                    lines,
                    color_output,
                    False,
                    per_finding_max_lines_limit,
                    per_line_max_chars_limit,
                    False,
                    is_different_file,
                )
                prev_path = path

        if sink:
            yield Text.assemble("")
            yield Text.assemble(
                FINDINGS_INDENT_DEPTH * " " + "This is how taint reaches the sink:"
            )
            yield from call_trace_to_lines(
                rule_match_path,
                sink,
                color_output,
                per_finding_max_lines_limit,
                per_line_max_chars_limit,
            )
            yield Text.assemble("")

        if source and show_separator:
            seperator_fill_count = safe_width(min(40, FINDINGS_TEXT_WIDTH - 1))
            yield Text.assemble(
                f" " * (FINDINGS_INDENT_DEPTH - 4) + f"⋮┆" + f"-" * seperator_fill_count
            )


def get_details_shortlink(rule_match: RuleMatch) -> Optional[str]:
    source_url = rule_match.metadata.get("shortlink")
    if not source_url:
        return ""
    return f"Details: {source_url}"


def print_time_summary(
    time_data: out.Profile, error_output: Sequence[SemgrepError]
) -> None:
    items_to_show = 5
    col_lim = 50

    targets = time_data.targets

    # Compute summary timings
    rule_parsing_time = time_data.rules_parse_time
    rule_match_timings = {
        rule.value: sum(t.match_times[i] for t in targets if t.match_times[i] >= 0)
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
        err for err in error_output if isinstance(err, SemgrepCoreError)
    ]
    errors = {
        (
            err.core.location.path.value if err.core.location else "",
            err.core.error_type.kind,
        )
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


def print_matches(
    rule_matches: Iterable[RuleMatch],
    color_output: bool,
    per_finding_max_lines_limit: Optional[int],
    per_line_max_chars_limit: Optional[int],
    dataflow_traces: bool,
) -> None:
    last_file = None
    last_rule_id = None
    last_message = None

    # Sort the findings according to RuleMatch.get_ordering_key()
    sorted_rule_matches = sorted(rule_matches)

    for rule_index, rule_match in enumerate(sorted_rule_matches):
        current_file = (
            f"{rule_match.path}@{rule_match.git_commit.value}"
            if rule_match.git_commit
            else rule_match.path
        )
        message = rule_match.message
        fix = rule_match.fix

        # reachable match adjustments
        lockfile: Optional[str] = None
        if rule_match.match.extra.sca_match and (
            rule_match.match.extra.sca_match.reachable
        ):
            lockfile = rule_match.match.extra.sca_match.dependency_match.lockfile.value

        if last_file is None or last_file != current_file:
            if last_file is not None:
                console.print()
            console.print(
                f"\n{with_color(Colors.cyan, f'  {current_file}', bold=False)}"
                + (
                    f" with lockfile {with_color(Colors.cyan, f'{lockfile}')}"
                    if lockfile
                    else ""
                )
            )
            last_rule_id = None
            last_message = None

        # don't display the rule line if the check is empty
        if (
            rule_match.rule_id
            and rule_match.rule_id != CLI_RULE_ID
            and (
                last_rule_id is None
                or last_rule_id != rule_match.rule_id
                or last_message is None
                or last_message != message
            )
        ):
            rule_title = (
                rule_match.title
            )  # Title of the rule that we need to bold later
            wrapped_text = textwrap.fill(
                rule_title,
                width=safe_width(RULE_TEXT_WIDTH),
                initial_indent="",
                subsequent_indent=RULE_INDENT * " ",
            )
            sev_color, sev_icon = to_severity_indicator(rule_match, color_output)
            text = Text.assemble(
                (RULE_INDENT - 4) * " ",
                (sev_icon, sev_color),
                " ",
                (f"{wrapped_text}", "bold"),
            )
            if last_file == current_file and last_rule_id != rule_match.rule_id:
                console.print(
                    " "
                )  # add a blank line between different rules in the same file

            console.print(text)

            # SCA severity addon
            severity = (
                (
                    f"{BASE_INDENT * ' '}Severity: {with_color(Colors.foreground, rule_match.metadata['sca-severity'], bold=True)}\n"
                )
                if rule_match.match.extra.sca_match
                and "sca-severity" in rule_match.metadata
                else ""
            )
            message_text = click.wrap_text(
                f"{message}",
                width=safe_width(DESC_TEXT_WIDTH),
                initial_indent=BASE_INDENT * " ",
                subsequent_indent=BASE_INDENT * " ",
                preserve_paragraphs=True,
            )

            # SCA transitive reachability addon
            transitivity = ""
            if (
                rule_match.match.extra.sca_match
                and rule_match.match.extra.sca_match.kind
            ):
                kind = rule_match.match.extra.sca_match.kind.value
                explanation = None
                # TODO: probably should also do something for the other kind
                if (
                    isinstance(kind, out.TransitiveReachable_)
                    and kind.value.explanation
                ):
                    explanation = kind.value.explanation
                if explanation:
                    transitivity = f"{BASE_INDENT * ' '}Transitivity: {with_color(Colors.foreground, explanation, bold=True)}\n"

            # Shortlink metadata addon
            shortlink = get_details_shortlink(rule_match)
            shortlink_text = (BASE_INDENT * " " + shortlink + "\n") if shortlink else ""
            console.print(f"{severity}{transitivity}{message_text}\n{shortlink_text}")

        if fix is not None:
            autofix_tag = "▶▶┆ Autofix ▶ "  # 13 chars for autofix tag
            wrapped_fix = (
                textwrap.fill(
                    textwrap.dedent(
                        " ".join(l.strip() for l in fix.splitlines(keepends=True))
                    ),
                    width=safe_width(AUTOFIX_TEXT_WIDTH),
                    initial_indent="",
                    subsequent_indent=(BASE_INDENT + 4) * " ",  # 4 for line number
                )
                if fix
                else ""  # keep as empty string if fix is empty string
            )
            fix_text = Text.assemble(
                (BASE_INDENT + 1) * " ",
                (autofix_tag, "green"),
                wrapped_fix if wrapped_fix else ("delete", "red"),
            )
            console.print(fix_text)
        elif (
            rule_match.match.extra.sca_match
            and "sca-fix-versions" in rule_match.metadata
            and (
                last_rule_id is None
                or last_rule_id != rule_match.rule_id
                or last_message is None
                or last_message != message
            )
        ):
            # this is a list of objects like [{'minimist': '0.2.4'}, {'minimist': '1.2.6'}]
            fixes = rule_match.metadata["sca-fix-versions"]
            # will be structure { 'package_name': set('1.2.3', '2.3.4') }
            dep_name = (
                rule_match.match.extra.sca_match.dependency_match.found_dependency.package
            )
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
        last_rule_id = rule_match.rule_id
        last_message = message
        next_rule_match = (
            sorted_rule_matches[rule_index + 1]
            if rule_index != len(sorted_rule_matches) - 1
            else None
        )
        is_same_file = (
            next_rule_match.path == rule_match.path if next_rule_match else False
        )
        is_same_rule = (
            next_rule_match.rule_id == rule_match.rule_id if next_rule_match else False
        )
        show_separator = (
            is_same_file
            and is_same_rule
            and not (dataflow_traces and rule_match.match.extra.dataflow_trace)
        )
        # Let's print the findings now
        for line in finding_to_line(
            rule_match,
            color_output,
            per_finding_max_lines_limit,
            per_line_max_chars_limit,
            # if we have dataflow traces on, then we should print the separator,
            # because otherwise it is easy to mistake taint traces as belonging
            # to a different finding
            show_separator,
        ):
            console.print(line)

        if dataflow_traces:
            for line in dataflow_trace_to_lines(
                rule_match.path,
                rule_match.match.extra.dataflow_trace,
                color_output,
                per_finding_max_lines_limit,
                per_line_max_chars_limit,
                is_same_file,
            ):
                console.print("  ", end="")
                console.print(line)


class TextFormatter(base.BaseFormatter):
    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        ctx: out.FormatContext,
    ) -> str:
        # all output in this function is captured and returned as a string
        with force_quiet_off(console), console.capture() as captured_output:
            grouped_matches: Dict[Tuple[out.Product, str], List[RuleMatch]] = {
                # ordered most important to least important
                (out.Product(out.SAST()), "blocking"): [],
                (out.Product(out.SCA()), "reachable"): [],
                (out.Product(out.Secrets()), "valid"): [],
                (out.Product(out.SCA()), "undetermined"): [],
                (out.Product(out.Secrets()), "validation error"): [],
                (out.Product(out.Secrets()), "unvalidated"): [],
                (out.Product(out.SCA()), "unreachable"): [],
                (out.Product(out.SAST()), "nonblocking"): [],
                (out.Product(out.Secrets()), "invalid"): [],
                (out.Product(out.Secrets()), "generic-secrets"): [],
            }

            secrets_blocking_rules = set()

            for match in rule_matches:
                product = match.product.value
                if isinstance(product, out.SAST):
                    subgroup = "blocking" if match.is_blocking else "nonblocking"
                elif isinstance(product, out.Secrets):
                    if match.is_blocking:
                        secrets_blocking_rules.add(match.match.check_id.value)
                    if match.metadata.get("generic_secrets", False):
                        subgroup = "generic-secrets"
                    else:
                        state = match.match.extra.validation_state
                        if state is None:
                            subgroup = "unvalidated"
                        else:
                            if isinstance(state.value, out.ConfirmedValid):
                                subgroup = "valid"
                            elif isinstance(state.value, out.ConfirmedInvalid):
                                subgroup = "invalid"
                            elif isinstance(state.value, out.ValidationError):
                                subgroup = "validation error"
                            else:
                                subgroup = "unvalidated"
                elif isinstance(product, out.SCA):
                    subgroup = match.exposure_type or "undetermined"
                    if match.match.extra.sca_match and match.match.extra.sca_match.kind:
                        kind = match.match.extra.sca_match.kind.value
                        if isinstance(kind, out.DirectReachable) or isinstance(
                            kind, out.TransitiveReachable_
                        ):
                            subgroup = "reachable"
                        elif isinstance(kind, out.DirectUnreachable) or isinstance(
                            kind, out.TransitiveUnreachable_
                        ):
                            subgroup = "unreachable"
                        elif isinstance(kind, out.TransitiveUndetermined_):
                            subgroup = "undetermined"
                        else:
                            logger.warning(f"Unknown match kind:{kind}")
                else:
                    logger.warning(f"Unknown product:{product}")
                    subgroup = "undetermined"

                grouped_matches[match.product, subgroup].append(match)

            code_blocking_rules = {
                match.match.check_id.value
                for match in grouped_matches[out.Product(out.SAST()), "blocking"]
            }

            # When ephemeral rules are run with the -e or --pattern flag in the command-line, the rule_id is set to -.
            # The rule is ran in the command-line and has no associated rule_id
            code_blocking_rules.discard("-")

            if not ctx.is_ci_invocation:
                grouped_matches[(out.Product(out.SAST()), "merged")] = [
                    *grouped_matches.pop((out.Product(out.SAST()), "nonblocking")),
                    *grouped_matches.pop((out.Product(out.SAST()), "blocking")),
                ]

            for group, matches in grouped_matches.items():
                if not matches:
                    continue

                # Generic Secrets findings are somewhat special, and don't print out their
                # matches in the output.
                # Instead, they are sent to the App for LLM validation. We expect this to
                # be noisy, so we won't print out all of the findings here.
                if group[1] == "generic-secrets" and ctx.is_ci_invocation:
                    url = get_state().env.semgrep_url
                    console.print(
                        textwrap.dedent(
                            f"""
                        Your deployment has generic secrets enabled. {len(matches)} potential line locations
                        will be uploaded to the Semgrep platform and then analyzed by Semgrep Assistant.
                        Any findings that appear actionable will be available in the Semgrep Platform.
                        You can view the secrets analyzed by Assistant at {url}/orgs/-/secrets?status=open&type=AI-detected+secret+%28beta%29
                        """
                        )
                    )
                else:
                    console.print(Title(unit_str(len(matches), GROUP_TITLES[group])))

                    print_matches(
                        matches,
                        extra.get("color_output", False),
                        extra["per_finding_max_lines_limit"],
                        extra["per_line_max_chars_limit"],
                        extra["dataflow_traces"],
                    )

            if code_blocking_rules and ctx.is_ci_invocation:
                console.print(Title("Blocking Code Rules Fired:", order=2))
                for rule_id in sorted(code_blocking_rules):
                    console.print(f"  {rule_id}")
                console.reset_title(order=1)

            if secrets_blocking_rules and ctx.is_ci_invocation:
                console.print(Title("Blocking Secrets Rules Fired:", order=2))
                for rule_id in sorted(secrets_blocking_rules):
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
