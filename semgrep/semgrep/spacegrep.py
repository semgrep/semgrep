import json
import logging
import subprocess
from pathlib import Path
from textwrap import dedent
from typing import Any
from typing import cast
from typing import Dict
from typing import List

from semgrep.constants import SPACEGREP_PATH
from semgrep.core_exception import CoreException
from semgrep.error import SemgrepError
from semgrep.pattern import Pattern
from semgrep.rule_lang import Position
from semgrep.util import sub_run

logger = logging.getLogger(__name__)


def _extract_matching_time(json: Dict[str, Any]) -> float:
    """Extract the matching time from the 'time' field of the spacegrep output.

    It is expected to have run a single pattern on a single target.
    """
    if "time" in json:
        res = json["time"]["targets"][0]["match_time"]
        if isinstance(res, float) or isinstance(res, int):
            return res
        else:
            return 0.0
    else:
        return 0.0


def run_spacegrep(
    rule_id: str,
    patterns: List[Pattern],
    targets: List[Path],
    timeout: int,
    report_time: bool,
) -> dict:
    matches: List[dict] = []
    errors: List[dict] = []
    targets_time: Dict[str, float] = {}
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
                "--timeout",
                str(timeout),
            ]
            if report_time:
                cmd += ["--time"]

            try:
                p = sub_run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                # exit code 3 indicates a timeout. See 'spacegrep --help'.
                if p.returncode == 3:
                    err = CoreException(
                        check_id="Timeout",
                        path=target,
                        start=Position(0, 0),
                        end=Position(0, 0),
                        extra={
                            "message": "spacegrep timeout",
                            "line": "",
                        },
                        language="generic",
                        rule_id=rule_id,
                    ).to_dict()
                    errors.append(err)
                else:
                    p.check_returncode()
                    raw_output = p.stdout

                    output_json = _parse_spacegrep_output(raw_output)
                    output_json["matches"] = _patch_id(
                        pattern, output_json.get("matches", [])
                    )
                    # dedent lines
                    for match in output_json["matches"]:
                        match["extra"]["lines"] = dedent(
                            "\n".join(
                                filter(None, match.get("extra", {}).get("lines", []))
                            )
                        ).split("\n")

                    matches.extend(output_json["matches"])
                    errors.extend(output_json["errors"])
                    # aggregate the match times obtained for the different patterns of the rule
                    path_s = str(target)
                    targets_time[path_s] = targets_time.get(
                        path_s, 0.0
                    ) + _extract_matching_time(output_json)

            except subprocess.CalledProcessError as e:
                raw_error = p.stderr
                spacegrep_error_text = raw_error.decode("utf-8", errors="replace")
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

    time = {
        "targets": [
            {"path": str(path), "match_time": targets_time.get(str(path), 0.0)}
            for path in targets
        ]
    }
    return {
        "matches": matches,
        "errors": errors,
        "time": time,
    }


def _parse_spacegrep_output(raw_output: bytes) -> dict:
    output = raw_output.decode("utf-8", errors="replace")
    data = json.loads(output)
    return cast(dict, data)


def _patch_id(pattern: Pattern, matches: List[dict]) -> List[dict]:
    patched = []
    for match in matches:
        match["check_id"] = pattern._id
        patched.append(match)
    return patched
