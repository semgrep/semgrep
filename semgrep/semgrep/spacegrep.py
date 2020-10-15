import logging
import re
import subprocess
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional

logger = logging.getLogger(__name__)

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SEMGREP_PATH
from semgrep.core_exception import CoreException
from semgrep.error import InvalidPatternError
from semgrep.pattern import Pattern
from semgrep.pattern_match import PatternMatch
from semgrep.util import sub_run


def run_spacegrep(patterns: List[Pattern], targets: List[Path]) -> dict:
    SPACEGREP_PATH = "spacegrep"
    for pattern in patterns:
        pattern_str = pattern._pattern  # TODO: Handle pattern Dict
        for target in targets:
            cmd = [
                SPACEGREP_PATH,
                "--debug",
                "-d",
                str(target),
                pattern_str,
            ]
            p = sub_run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            raw_output = p.stdout
            raw_errors = p.stderr

            matches = _parse_spacegrep_output(pattern, raw_output)
            errors = _parse_spacegrep_errors(raw_errors)

    return {
        "matches": matches,
        "errors": errors,
    }


def _parse_spacegrep_output(pattern: Pattern, raw_output: bytes) -> List[dict]:
    findings = raw_output.decode("utf-8").split("\n\n")
    returns = []
    for finding in filter(None, findings):
        match_text = finding.split("\n", maxsplit=1)[1]
        filename, match = match_text.split(":", maxsplit=1)
        match = "\n".join(
            [line.replace(f"{filename}:", "", 1) for line in match.split("\n")]
        )
        match_lines = [int(n) for n in set(re.findall("lnum=(\d+)", finding))]
        match_offsets = [int(n) for n in set(re.findall("cnum=(\d+)", finding))]
        returns.append(
            {
                "check_id": pattern._id,
                "path": filename,
                "start": {
                    "offset": min(match_offsets),
                    "line": min(match_lines),
                    "col": 0,
                },
                "end": {
                    "offset": min(match_offsets) + len(match),
                    "line": max(match_lines) + 1,
                    "col": len(match),
                },
                "extra": {"lines": [match]},
            }
        )
    return returns


def _parse_spacegrep_errors(raw_errors: bytes) -> List[CoreException]:
    return []
