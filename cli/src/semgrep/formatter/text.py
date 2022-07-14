import textwrap
from itertools import groupby
from pathlib import Path
from shutil import get_terminal_size
from typing import Any
from typing import cast
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence

import click
import colorama

import semgrep.semgrep_interfaces.semgrep_output_v0 as out
from semgrep.constants import CLI_RULE_ID
from semgrep.constants import Colors
from semgrep.constants import ELLIPSIS_STRING
from semgrep.constants import MAX_CHARS_FLAG_NAME
from semgrep.constants import MAX_LINES_FLAG_NAME
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import LANGUAGE
from semgrep.semgrep_types import Language
from semgrep.util import format_bytes
from semgrep.util import truncate
from semgrep.util import unit_str
from semgrep.util import with_color

MAX_TEXT_WIDTH = 120

terminal_size = get_terminal_size((MAX_TEXT_WIDTH, 1))[0]
if terminal_size <= 0:
    terminal_size = MAX_TEXT_WIDTH
width = min(MAX_TEXT_WIDTH, terminal_size)
if width <= 110:
    width = width - 5
else:
    width = width - (width - 100)

FINDINGS_INDENT_DEPTH = 10


class TextFormatter(BaseFormatter):
    @staticmethod
    def _color_line(
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

    @staticmethod
    def _finding_to_line(
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
        trimmed = 0
        stripped = False
        if path:
            lines = rule_match.extra.get("fixed_lines") or rule_match.lines

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
                        line = TextFormatter._color_line(
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

                    if (
                        per_line_max_chars_limit
                        and len(line) > per_line_max_chars_limit
                    ):
                        stripped = True
                        is_first_line = i == 0
                        if is_first_line:
                            line = (
                                line[
                                    start_col
                                    - 1 : start_col
                                    - 1
                                    + per_line_max_chars_limit
                                ]
                                + ELLIPSIS_STRING
                            )
                            if start_col > 1:
                                line = ELLIPSIS_STRING + line
                        else:
                            line = line[:per_line_max_chars_limit] + ELLIPSIS_STRING
                        # while stripping a string, the ANSI code for resetting color might also get stripped.
                        line = line + colorama.Style.RESET_ALL

                yield f" " * (
                    11 - len(line_number)
                ) + f"{line_number}┆ {line}" if line_number else f"{line}"

            if stripped:
                stripped_str = f"[shortened a long line from output, adjust with {MAX_CHARS_FLAG_NAME}]"
                yield " " * FINDINGS_INDENT_DEPTH + stripped_str

            if per_finding_max_lines_limit != 1:
                if trimmed > 0:
                    trimmed_str = f" [hid {trimmed} additional lines, adjust with {MAX_LINES_FLAG_NAME}] "
                    yield " " * FINDINGS_INDENT_DEPTH + trimmed_str
                elif lines and show_separator:
                    yield f" " * FINDINGS_INDENT_DEPTH + f"⋮┆" + f"-" * 40

    @staticmethod
    def _get_details_shortlink(rule_match: RuleMatch) -> Optional[str]:
        source_url = rule_match.metadata.get("shortlink")
        if not source_url:
            return ""
        return f"Details: {source_url}"

    @staticmethod
    def _build_summary(
        time_data: out.CliTiming,
        error_output: Sequence[SemgrepError],
        color_output: bool,
    ) -> Iterator[str]:
        items_to_show = 5
        col_lim = 50

        targets = time_data.targets

        # Compute summary timings
        rule_parsing_time = time_data.rules_parse_time
        rule_match_timings = {
            rule.id.value: sum(
                t.match_times[i] for t in targets if t.match_times[i] >= 0
            )
            for i, rule in enumerate(time_data.rules)
        }
        file_parsing_time = sum(
            sum(t for t in target.parse_times if t >= 0) for target in targets
        )
        file_timings = {
            target.path: (
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
            (err.core.location.path, err.core.error_type.kind)
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
                    lang_of_path(target.path),
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

        yield f"\n============================[ summary ]============================"

        yield f"Total time: {total_time:.4f}s Config time: {config_time:.4f}s Core time: {core_time:.4f}s"

        # Output semgrep-core information
        yield f"\nSemgrep-core time:"
        yield f"Total CPU time: {all_total_time:.4f}s  File parse time: {file_parsing_time:.4f}s" f"  Rule parse time: {rule_parsing_time:.4f}s  Match time: {total_matching_time:.4f}s"

        yield f"Slowest {items_to_show}/{len(file_timings)} files"
        slowest_file_times = sorted(
            file_timings.items(), key=lambda x: x[1], reverse=True
        )[:items_to_show]
        for file_name, (parse_time, run_time) in slowest_file_times:
            num_bytes = f"({format_bytes(Path(file_name).resolve().stat().st_size)}):"
            file_name = truncate(file_name, col_lim)
            yield f"{with_color(Colors.green, f'{file_name:<50}')} {num_bytes:<8} {run_time:.3f}s ({parse_time:.3f}s to parse)"

        yield f"Slowest {items_to_show} rules to match"
        slowest_rule_times = sorted(rule_match_timings.items(), reverse=True)[
            :items_to_show
        ]
        for rule_id, match_time in slowest_rule_times:
            rule_id = truncate(rule_id, col_lim) + ":"
            yield f"{with_color(Colors.yellow, f'{rule_id:<59}')} {match_time:.3f}s"

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

        yield ""

        by_lang = [
            f"{ lang_counts[lang] } { lang } files ({ format_bytes(lang_bytes[lang]) } in {(lang_times[lang]):.3f} seconds)"
            for lang in langs
        ]
        yield from add_heading(ANALYZED, by_lang)

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

        yield from add_heading(FAILED, error_lines)

        yield ""

    @staticmethod
    def _build_text_output(
        rule_matches: Iterable[RuleMatch],
        color_output: bool,
        per_finding_max_lines_limit: Optional[int],
        per_line_max_chars_limit: Optional[int],
    ) -> Iterator[str]:

        last_file = None
        last_message = None
        sorted_rule_matches = sorted(rule_matches, key=lambda r: (r.path, r.rule_id))
        for rule_index, rule_match in enumerate(sorted_rule_matches):

            current_file = rule_match.path
            rule_id = rule_match.rule_id
            message = rule_match.message
            fix = rule_match.fix
            if "dependency_matches" in rule_match.extra and (
                not rule_match.extra.get("dependency_match_only", True)
            ):
                lockfile = rule_match.extra["dependency_matches"][0]["lockfile"]
            else:
                lockfile = None
            if last_file is None or last_file != current_file:
                if last_file is not None:
                    yield ""
                yield f"\n{with_color(Colors.cyan, f'  {current_file} ', bold=False)}" + (
                    f"with lockfile {with_color(Colors.cyan, f'{lockfile}')}"
                    if lockfile
                    else ""
                )
                last_message = None
            # don't display the rule line if the check is empty
            if (
                rule_id
                and rule_id != CLI_RULE_ID
                and (last_message is None or last_message != message)
            ):
                shortlink = TextFormatter._get_details_shortlink(rule_match)
                shortlink_text = (8 * " " + shortlink + "\n") if shortlink else ""
                rule_id_text = click.wrap_text(
                    f"{with_color(Colors.foreground, rule_id, bold=True)}",
                    width + 10,
                    5 * " ",
                    5 * " ",
                    False,
                )
                message_text = click.wrap_text(
                    f"{message}", width, 8 * " ", 8 * " ", True
                )
                yield f"{rule_id_text}\n{message_text}\n{shortlink_text}"

            last_file = current_file
            last_message = message
            next_rule_match = (
                sorted_rule_matches[rule_index + 1]
                if rule_index != len(sorted_rule_matches) - 1
                else None
            )
            autofix_tag = with_color(Colors.green, "         ▶▶┆ Autofix ▶")
            if fix:
                yield f"{autofix_tag} {fix}"
            elif rule_match.fix_regex:
                fix_regex = rule_match.fix_regex
                yield f"{autofix_tag} s/{fix_regex.regex}/{fix_regex.replacement}/{fix_regex.count or 'g'}"

            is_same_file = (
                next_rule_match.path == rule_match.path if next_rule_match else False
            )
            yield from TextFormatter._finding_to_line(
                rule_match,
                color_output,
                per_finding_max_lines_limit,
                per_line_max_chars_limit,
                is_same_file,
            )

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
    ) -> str:
        reachable = []
        unreachable = []
        first_party = []
        for match in rule_matches:
            if "dependency_match_only" not in match.extra:
                first_party.append(match)
            elif match.extra["dependency_match_only"]:
                unreachable.append(match)
            else:
                reachable.append(match)

        timing_output = (
            self._build_summary(
                cli_output_extra.time,
                semgrep_structured_errors,
                extra.get("color_output", False),
            )
            if cli_output_extra.time
            else iter([])
        )

        findings_output = []
        if reachable or unreachable:
            findings_output.append(
                f"\n{with_color(Colors.foreground, 'SCA Summary')}: {unit_str(len(reachable),with_color(Colors.red,'Reachable finding'))}, {unit_str(len(unreachable),with_color(Colors.yellow,'Unreachable finding'))}\n"
            )
        if reachable:
            reachable_output = self._build_text_output(
                reachable,
                extra.get("color_output", False),
                extra["per_finding_max_lines_limit"],
                extra["per_line_max_chars_limit"],
            )

            findings_output.append(
                f"\n{with_color(Colors.red, 'Reachable SCA Findings:')}\n"
                + "\n".join(reachable_output)
            )

        if unreachable:
            unreachable_output = self._build_text_output(
                unreachable,
                extra.get("color_output", False),
                extra["per_finding_max_lines_limit"],
                extra["per_line_max_chars_limit"],
            )

            findings_output.append(
                f"\n{with_color(Colors.yellow, 'Unreachable SCA Findings:')}\n"
                + "\n".join(unreachable_output)
            )

        if (reachable or unreachable) and first_party:
            first_party_output = self._build_text_output(
                first_party,
                extra.get("color_output", False),
                extra["per_finding_max_lines_limit"],
                extra["per_line_max_chars_limit"],
            )
            findings_output.append(
                "\nFirst-Party Findings:\n" + "\n".join(first_party_output)
            )
        elif first_party:
            first_party_output = self._build_text_output(
                first_party,
                extra.get("color_output", False),
                extra["per_finding_max_lines_limit"],
                extra["per_line_max_chars_limit"],
            )
            findings_output.append("\nFindings:\n" + "\n".join(first_party_output))

        return "\n".join(
            [
                *findings_output,
                *timing_output,
            ]
        )
