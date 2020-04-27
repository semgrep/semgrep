import itertools
import json
from pathlib import Path
from typing import Any
from typing import Dict
from typing import Iterable
from typing import Iterator
from typing import Optional

import colorama

from semgrep.util import fetch_lines_in_file


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


def finding_to_line(finding: Dict[str, Any], color_output: bool) -> Iterator[str]:
    path = finding.get("path")
    start_line = finding.get("start", {}).get("line")
    end_line = finding.get("end", {}).get("line")
    start_col = finding.get("start", {}).get("col")
    end_col = finding.get("end", {}).get("col")
    if path and start_line:
        file_lines = fetch_lines_in_file(Path(path), start_line, end_line)
        if file_lines:
            for i, line in enumerate(file_lines):
                if color_output:
                    yield f"{colorama.Fore.GREEN}{start_line + i}{colorama.Style.RESET_ALL}:{color_line(line.rstrip(), start_line + i, start_line, start_col, end_line, end_col)}"
                else:
                    yield f"{start_line + i}:{line.rstrip()}"


def build_normal_output(
    output_data: Dict[str, Any], color_output: bool
) -> Iterator[str]:
    results = output_data.get("results", [])
    last_file = None
    last_message = None
    for finding in sorted(
        results,
        key=lambda k: (k.get("path", "<no path>"), k.get("check_id", "<no rule id>")),
    ):
        RESET_COLOR = colorama.Style.RESET_ALL if color_output else ""
        GREEN_COLOR = colorama.Fore.GREEN if color_output else ""
        YELLOW_COLOR = colorama.Fore.YELLOW if color_output else ""
        BLUE_COLOR = colorama.Fore.BLUE if color_output else ""

        current_file = finding.get("path", "<no path>")
        check_id = finding.get("check_id")
        extra = finding.get("extra", {})
        message = extra.get("message")
        fix = extra.get("fix")
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
            yield f"{YELLOW_COLOR}rule:{check_id}: {finding.get('extra', {}).get('message')}{RESET_COLOR}"

        last_file = current_file
        last_message = message
        yield from finding_to_line(finding, color_output)
        if fix:
            yield f"{BLUE_COLOR}autofix:{RESET_COLOR} {fix}"


def build_output_json(output_json: Dict[str, Any]) -> str:
    # wrap errors under "data" entry to be compatible with
    # https://docs.r2c.dev/en/latest/api/output.html#errors
    errors = output_json["errors"]
    if errors:
        output_json["errors"] = {
            "data": {"errors": output_json["errors"]},
            "message": "SgrepRuntimeErrors",
        }
    return json.dumps(output_json)
