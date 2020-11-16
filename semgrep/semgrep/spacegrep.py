import json
import logging
import re
import subprocess
from pathlib import Path
from typing import Any
from typing import cast
from typing import Dict
from typing import List
from typing import Optional

logger = logging.getLogger(__name__)

from semgrep.constants import PLEASE_FILE_ISSUE_TEXT
from semgrep.constants import SPACEGREP_PATH
from semgrep.core_exception import CoreException
from semgrep.error import InvalidPatternError
from semgrep.error import SemgrepError
from semgrep.pattern import Pattern
from semgrep.pattern_match import PatternMatch
from semgrep.util import sub_run


def run_spacegrep(patterns: List[Pattern], targets: List[Path]) -> dict:
    matches: List[dict] = []
    errors: List[dict] = []
    for pattern in patterns:
        if not isinstance(pattern._pattern, str):
            raise NotImplementedError(
                f"Support for {type(pattern._pattern)} has not been implemented yet."
            )
        pattern_str = pattern._pattern  # TODO: Handle pattern Dict
        for target in targets:
            cmd = [
                SPACEGREP_PATH,
                "--output-format",
                "semgrep",
                "-d",
                str(target),
                pattern_str,
            ]
            try:
                p = sub_run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                p.check_returncode()
                raw_output = p.stdout

                output_json = _parse_spacegrep_output(raw_output)
                output_json["matches"] = _patch_id(
                    pattern, output_json.get("matches", [])
                )

                matches.extend(output_json["matches"])
                errors.extend(output_json["errors"])
            except subprocess.CalledProcessError as e:
                raw_error = p.stderr
                spacegrep_error_text = raw_error.decode("utf-8")
                raise SemgrepError(
                    f"Error running spacegrep on file {target}: Process error: {e}\n\nspacegrep error: {spacegrep_error_text}"
                )
            except json.JSONDecodeError as e:
                raise SemgrepError(
                    f"Could not parse spacegrep output as JSON: JSON error: {e}"
                )
            except KeyError as e:
                raise SemgrepError(
                    f"Invalid JSON output was received from spacegrep: {e}"
                )

    return {
        "matches": matches,
        "errors": errors,
    }


def _parse_spacegrep_output(raw_output: bytes) -> dict:
    output = raw_output.decode("utf-8")
    data = json.loads(output)
    return cast(dict, data)


def _patch_id(pattern: Pattern, matches: List[dict]) -> List[dict]:
    patched = []
    for match in matches:
        match["check_id"] = pattern._id
        patched.append(match)
    return patched
