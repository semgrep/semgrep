import functools
import itertools
from itertools import groupby
from pathlib import Path
from typing import Any
from typing import cast
from typing import Iterable
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional
from typing import Sequence

import colorama

from semgrep.constants import BREAK_LINE
from semgrep.constants import BREAK_LINE_CHAR
from semgrep.constants import BREAK_LINE_WIDTH
from semgrep.constants import CLI_RULE_ID
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
from semgrep.util import with_color


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
        start_color = 0 if line_number > start_line else start_col
        # column offset
        start_color = max(start_color - 1, 0)
        end_color = end_col if line_number >= end_line else len(line) + 1 + 1
        end_color = max(end_color - 1, 0)
        line = (
            line[:start_color]
            + with_color(
                "bright_black", line[start_color : end_color + 1]
            )  # want the color to include the end_col
            + line[end_color + 1 :]
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

            for i, line in enumerate(lines):
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
                        line_number = with_color("green", f"{start_line + i}")
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

                yield f"{line_number}:{line}" if line_number else f"{line}"

            if stripped:
                yield f"[Shortened a long line from output, adjust with {MAX_CHARS_FLAG_NAME}]"
            trimmed_str = (
                f" [hid {trimmed} additional lines, adjust with {MAX_LINES_FLAG_NAME}] "
            )
            if per_finding_max_lines_limit != 1:
                if trimmed > 0:
                    yield trimmed_str.center(BREAK_LINE_WIDTH, BREAK_LINE_CHAR)
                elif show_separator:
                    yield BREAK_LINE

    @staticmethod
    def _get_details_shortlink(rule_match: RuleMatch) -> Optional[str]:
        source_url = rule_match._metadata.get("shortlink")
        if not source_url:
            return ""
        return f' Details: {with_color("bright_blue", source_url)}'

    @staticmethod
    def _build_summary(
        time_data: Mapping[str, Any],
        error_output: Sequence[SemgrepError],
        color_output: bool,
    ) -> Iterator[str]:
        items_to_show = 5
        col_lim = 70

        targets = time_data["targets"]

        # Compute summary timings
        rule_parsing_time = sum(
            parse_time for parse_time in time_data["rule_parse_info"]
        )
        rule_timings = {
            rule["id"]: functools.reduce(
                lambda x, y: (x[0] + y[0], x[1] + y[1]),
                (
                    (t["run_times"][i] - t["parse_times"][i], t["match_times"][i])
                    for t in targets
                ),
                (time_data["rule_parse_info"][i], 0.0),
            )
            for i, rule in enumerate(time_data["rules"])
        }
        file_parsing_time = sum(
            sum(t for t in target["parse_times"] if t >= 0) for target in targets
        )
        file_timings = {
            target["path"]: float(sum(t for t in target["run_times"] if t >= 0))
            for target in targets
        }

        all_total_time = sum(i for i in file_timings.values()) + rule_parsing_time
        total_matching_time = sum(i[1] for i in rule_timings.values() if i[1] >= 0)

        # Count errors

        semgrep_core_errors = [
            cast(SemgrepCoreError, err)
            for err in error_output
            if SemgrepError.semgrep_error_type(err) == "SemgrepCoreError"
        ]
        errors = {(err.path, err.error_type) for err in semgrep_core_errors}

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
            [
                (
                    lang_of_path(target["path"]),
                    (target["num_bytes"], sum(target["run_times"])),
                )
                for target in targets
            ],
            key=lambda x: x[0],
        )
        lang_info = {k: list(v) for k, v in groupby(ext_info, lambda x: x[0])}
        langs = lang_info.keys()
        lang_counts: Mapping[str, int] = {lang: len(lang_info[lang]) for lang in langs}
        lang_bytes: Mapping[str, int] = {
            lang: sum(info[1][0] for info in lang_info[lang]) for lang in langs
        }
        lang_times: Mapping[str, int] = {
            lang: sum(info[1][1] for info in lang_info[lang]) for lang in langs
        }

        # Output semgrep summary
        total_time = time_data["profiling_times"].get("total_time", 0.0)
        config_time = time_data["profiling_times"].get("config_time", 0.0)
        core_time = time_data["profiling_times"].get("core_time", 0.0)
        ignores_time = time_data["profiling_times"].get("ignores_time", 0.0)

        yield f"\n============================[ summary ]============================"

        yield f"Total time: {total_time:.4f} Config time: {config_time:.4f} Core time: {core_time:.4f} Ignores time: {ignores_time:.4f}"

        # Output semgrep-core information
        yield f"\nSemgrep-core time:"
        yield f"Total CPU time: {all_total_time:.4f}  File parse time: {file_parsing_time:.4f}" f"  Rule parse time: {rule_parsing_time:.4f}  Match time: {total_matching_time:.4f}"

        yield f"Slowest {items_to_show}/{len(file_timings)} files"
        slowest_file_times = sorted(
            file_timings.items(), key=lambda x: x[1], reverse=True
        )[:items_to_show]
        for file_name, parse_time in slowest_file_times:
            num_bytes = f"({format_bytes(Path(file_name).resolve().stat().st_size)}):"
            file_name = truncate(file_name, col_lim)
            yield f"{with_color('green', f'{file_name:<70}')} {num_bytes:<9}{parse_time:.4f}"

        yield f"Slowest {items_to_show} rules to run (excluding parse time)"
        slowest_rule_times = sorted(
            rule_timings.items(), key=lambda x: float(x[1][0]), reverse=True
        )[:items_to_show]
        for rule_id, (total_time, match_time) in slowest_rule_times:
            rule_id = truncate(rule_id, col_lim) + ":"
            yield f"{with_color('yellow', f'{rule_id:<71}')} run time {total_time:.4f}  match time {match_time:.4f}"

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
        for line in add_heading(ANALYZED, by_lang):
            yield line

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

        for line in add_heading(FAILED, error_lines):
            yield line

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
        sorted_rule_matches = sorted(rule_matches, key=lambda r: (r.path, r.id))
        for rule_index, rule_match in enumerate(sorted_rule_matches):

            current_file = rule_match.path
            check_id = rule_match.id
            message = rule_match.message
            fix = rule_match.fix
            if last_file is None or last_file != current_file:
                if last_file is not None:
                    yield ""
                yield with_color("green", str(current_file))
                last_message = None
            # don't display the rule line if the check is empty
            if (
                check_id
                and check_id != CLI_RULE_ID
                and (last_message is None or last_message != message)
            ):
                shortlink = TextFormatter._get_details_shortlink(rule_match)
                yield f"{with_color('yellow', f'rule:{check_id}: {message}{shortlink}')}"

            last_file = current_file
            last_message = message
            next_rule_match = (
                sorted_rule_matches[rule_index + 1]
                if rule_index != len(sorted_rule_matches) - 1
                else None
            )

            if fix:
                yield f"{with_color('bright_blue', 'autofix:')} {fix}"
            elif rule_match.fix_regex:
                fix_regex = rule_match.fix_regex
                yield f"{with_color('bright_blue', 'autofix:')} s/{fix_regex.get('regex')}/{fix_regex.get('replacement')}/{fix_regex.get('count', 'g')}"

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
        extra: Mapping[str, Any],
    ) -> str:
        output = self._build_text_output(
            rule_matches,
            extra.get("color_output", False),
            extra["per_finding_max_lines_limit"],
            extra["per_line_max_chars_limit"],
        )

        timing_output = (
            self._build_summary(
                extra.get("time", {}),
                semgrep_structured_errors,
                extra.get("color_output", False),
            )
            if "time" in extra
            else iter([])
        )

        return "\n".join(itertools.chain(output, timing_output))
