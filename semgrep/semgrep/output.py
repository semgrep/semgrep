import json
from typing import Any
from typing import Dict
from typing import FrozenSet
from typing import Iterator
from typing import List
from typing import Optional

import colorama

from semgrep.constants import __VERSION__
from semgrep.constants import OutputFormat
from semgrep.rule import Rule
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
    if path:
        for i, line in enumerate(rule_match.lines):
            line = line.rstrip()
            line_number = ""
            if start_line:
                if color_output:
                    line = color_line(
                        line, start_line + i, start_line, start_col, end_line, end_col  # type: ignore
                    )
                    line_number = f"{colorama.Fore.GREEN}{start_line + i}{colorama.Style.RESET_ALL}"
                else:
                    line_number = f"{start_line + i}"

            yield f"{line_number}:{line}" if line_number else f"{line}"


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
        message = rule_match.message
        severity = rule_match.severity.lower()
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
                if severity == "error":
                    severity_prepend = f"{RED_COLOR}severity:{severity} "
                elif severity == "warning":
                    severity_prepend = f"{YELLOW_COLOR}severity:{severity} "
                else:
                    severity_prepend = f"severity:{severity} "
            yield f"{severity_prepend}{YELLOW_COLOR}rule:{check_id}: {message}{RESET_COLOR}"

        last_file = current_file
        last_message = message
        yield from finding_to_line(rule_match, color_output)
        if fix:
            yield f"{BLUE_COLOR}autofix:{RESET_COLOR} {fix}"


def build_output_json(
    rule_matches: List[RuleMatch],
    semgrep_errors: List[Any],
    debug_steps_by_rule: Optional[Dict[Rule, List[Dict[str, Any]]]] = None,
) -> str:
    # wrap errors under "data" entry to be compatible with
    # https://docs.r2c.dev/en/latest/api/output.html#errors
    output_json = {}
    output_json["results"] = [rm.to_json() for rm in rule_matches]
    if debug_steps_by_rule:
        output_json["debug"] = [
            {r.id: steps for r, steps in debug_steps_by_rule.items()}
        ]
    output_json["errors"] = [
        {"data": e, "message": "SemgrepCoreRuntimeErrors"} for e in semgrep_errors
    ]
    return json.dumps(output_json)


def _sarif_tool_info() -> Dict[str, Any]:
    return {"name": "semgrep", "semanticVersion": __VERSION__}


def build_sarif_output(
    rule_matches: List[RuleMatch], rules: FrozenSet[Rule], semgrep_errors: List[Any]
) -> str:
    """
    Format matches in SARIF v2.1.0 formatted JSON.

    - written based on https://help.github.com/en/github/finding-security-vulnerabilities-and-errors-in-your-code/about-sarif-support-for-code-scanning
    - which links to this schema https://github.com/oasis-tcs/sarif-spec/blob/master/Schemata/sarif-schema-2.1.0.json
    - full spec is at https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html
    """
    output_dict = {
        "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
        "version": "2.1.0",
        "tool": {
            "driver": {
                **_sarif_tool_info(),
                "rules": [rule.to_sarif() for rule in rules],
            }
        },
        "results": [match.to_sarif() for match in rule_matches],
    }
    return json.dumps(output_dict)


def build_output(
    rule_matches: List[RuleMatch],
    debug_steps_by_rule: Dict[Rule, List[Dict[str, Any]]],
    rules: FrozenSet[Rule],
    semgrep_errors: List[Any],
    output_format: OutputFormat,
    color_output: bool,
) -> str:
    if output_format == OutputFormat.JSON_DEBUG:
        return build_output_json(rule_matches, semgrep_errors, debug_steps_by_rule)
    if output_format == OutputFormat.JSON:
        return build_output_json(rule_matches, semgrep_errors)
    elif output_format == OutputFormat.SARIF:
        return build_sarif_output(rule_matches, rules, semgrep_errors)
    elif output_format == OutputFormat.TEXT:
        return "\n".join(
            build_normal_output(rule_matches, semgrep_errors, color_output)
        )
    else:
        # https://github.com/python/mypy/issues/6366
        raise RuntimeError(f"Unhandled output format: {type(output_format).__name__}")
