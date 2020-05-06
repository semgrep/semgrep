import json
from typing import Any
from typing import Iterator
from typing import List

import colorama

from semgrep.rule_match import RuleMatch


def color_line(
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
        + colorama.Style.BRIGHT
        + line[start_color : end_color + 1]  # want the color to include the end_col
        + colorama.Style.RESET_ALL
        + line[end_color + 1 :]
    )
    return line


def finding_to_line(rule_match: RuleMatch, color_output: bool) -> Iterator[str]:
    path = rule_match.path
    start_line = rule_match.start.get("line")
    end_line = rule_match.end.get("line")
    start_col = rule_match.start.get("col")
    end_col = rule_match.end.get("col")
    if path and start_line:
        file_lines = rule_match.lines
        if file_lines:
            for i, line in enumerate(file_lines):
                if color_output:
                    yield f"{colorama.Fore.GREEN}{start_line + i}{colorama.Style.RESET_ALL}:{color_line(line.rstrip(), start_line + i, start_line, start_col, end_line, end_col)}"  # type: ignore
                else:
                    yield f"{start_line + i}:{line.rstrip()}"


def build_normal_output(
    rule_matches: List[RuleMatch], semgrep_errors: List[Any], color_output: bool
) -> Iterator[str]:
    RESET_COLOR = colorama.Style.RESET_ALL if color_output else ""
    GREEN_COLOR = colorama.Fore.GREEN if color_output else ""
    YELLOW_COLOR = colorama.Fore.YELLOW if color_output else ""
    RED_COLOR = colorama.Fore.RED if color_output else ""
    BLUE_COLOR = colorama.Fore.BLUE if color_output else ""

    last_file = None
    last_message = None
    for rule_match in sorted(rule_matches, key=lambda r: (r.path, r.id),):

        current_file = rule_match.path
        check_id = rule_match.id
        extra = rule_match.extra
        message = rule_match.message
        severity = rule_match.severity
        fix = rule_match.fix
        if last_file is None or last_file != current_file:
            if last_file is not None:
                yield ""
            yield f"{GREEN_COLOR}{current_file}{RESET_COLOR}"
            last_message = None
        # don't display the rule line if the check is empty
        if (
            check_id
            and check_id != "-"
            and (last_message is None or last_message != message)
        ):
            severity_prepend = ""
            if severity:
                if severity == "ERROR":
                    severity_prepend = f"{RED_COLOR}{severity} "
                elif severity == "WARNING":
                    severity_prepend = f"{YELLOW_COLOR}{severity} "
                else:
                    severity_prepend = f"{severity} "
            yield f"{severity_prepend}{YELLOW_COLOR}rule:{check_id}: {message}{RESET_COLOR}"

        last_file = current_file
        last_message = message
        yield from finding_to_line(rule_match, color_output)
        if fix:
            yield f"{BLUE_COLOR}autofix:{RESET_COLOR} {fix}"


def build_output_json(rule_matches: List[RuleMatch], semgrep_errors: List[Any]) -> str:
    # wrap errors under "data" entry to be compatible with
    # https://docs.r2c.dev/en/latest/api/output.html#errors
    output_json = {}
    output_json["results"] = [rm.to_json() for rm in rule_matches]

    errors = []
    for error in semgrep_errors:
        errors.append(
            {"data": semgrep_errors, "message": "SemgrepCoreRuntimeErrors",}
        )
    output_json["errors"] = errors
    return json.dumps(output_json)


def build_output(
    rule_matches: List[RuleMatch],
    semgrep_errors: List[Any],
    json_format: bool,
    color_output: bool,
) -> str:
    if json_format:
        return build_output_json(rule_matches, semgrep_errors)
    else:
        return "\n".join(
            build_normal_output(rule_matches, semgrep_errors, color_output)
        )
