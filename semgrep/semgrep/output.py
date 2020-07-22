import contextlib
import json
import logging
import sys
from pathlib import Path
from typing import Any
from typing import Dict
from typing import FrozenSet
from typing import Generator
from typing import IO
from typing import Iterator
from typing import List
from typing import NamedTuple
from typing import Optional
from typing import Set

import colorama

from semgrep import config_resolver
from semgrep.constants import __VERSION__
from semgrep.constants import OutputFormat
from semgrep.core_exception import CoreException
from semgrep.error import FINDINGS_EXIT_CODE
from semgrep.error import Level
from semgrep.error import SemgrepError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.util import is_url

logger = logging.getLogger(__name__)


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
        lines = rule_match.extra.get("fixed_lines") or rule_match.lines
        for i, line in enumerate(lines):
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
    rule_matches: List[RuleMatch], color_output: bool
) -> Iterator[str]:
    RESET_COLOR = colorama.Style.RESET_ALL if color_output else ""
    GREEN_COLOR = colorama.Fore.GREEN if color_output else ""
    YELLOW_COLOR = colorama.Fore.YELLOW if color_output else ""
    RED_COLOR = colorama.Fore.RED if color_output else ""
    BLUE_COLOR = colorama.Fore.BLUE if color_output else ""

    last_file = None
    last_message = None
    for rule_match in sorted(rule_matches, key=lambda r: (r.path, r.id)):

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
        elif rule_match.fix_regex:
            fix_regex = rule_match.fix_regex
            yield f"{BLUE_COLOR}autofix:{RESET_COLOR} s/{fix_regex.get('regex')}/{fix_regex.get('replacement')}/{fix_regex.get('count', 'g')}"


def build_output_json(
    rule_matches: List[RuleMatch],
    semgrep_errors: List[Any],
    semgrep_structured_errors: List[SemgrepError],
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
    ] + [e.to_dict() for e in semgrep_structured_errors]
    return json.dumps(output_json)


def _sarif_tool_info() -> Dict[str, Any]:
    return {"name": "semgrep", "semanticVersion": __VERSION__}


def build_sarif_output(rule_matches: List[RuleMatch], rules: FrozenSet[Rule]) -> str:
    """
    Format matches in SARIF v2.1.0 formatted JSON.

    - written based on https://help.github.com/en/github/finding-security-vulnerabilities-and-errors-in-your-code/about-sarif-support-for-code-scanning
    - which links to this schema https://github.com/oasis-tcs/sarif-spec/blob/master/Schemata/sarif-schema-2.1.0.json
    - full spec is at https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html
    """
    output_dict = {
        "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
        "version": "2.1.0",
        "runs": [
            {
                "tool": {
                    "driver": {
                        **_sarif_tool_info(),
                        "rules": [rule.to_sarif() for rule in rules],
                    }
                },
                "results": [match.to_sarif() for match in rule_matches],
            }
        ],
    }
    return json.dumps(output_dict)


class OutputSettings(NamedTuple):
    output_format: OutputFormat
    output_destination: Optional[str]
    error_on_findings: bool
    strict: bool


@contextlib.contextmanager
def managed_output(output_settings: OutputSettings) -> Generator:  # type: ignore
    """
    Context manager to capture uncaught exceptions &
    """
    output_handler = OutputHandler(output_settings)
    try:
        yield output_handler
    except Exception as ex:
        output_handler.handle_unhandled_exception(ex)
    finally:
        output_handler.close()


class OutputHandler:
    """
    Handle all output in a central location. Rather than calling `print_stderr` directly,
    you should call `handle_*` as appropriate.

    In normal usage, it should be constructed via the contextmanager, `managed_output`. It ensures that everything
    is handled properly if exceptions are thrown.

    If you need to stop execution immediately (think carefully if you really want this!), throw an exception.
    If this is normal behavior, the exception _must_ inherit from `SemgrepError`.

    If you want execution to continue, _report_ the exception via the appropriate `handle_*` method.
    """

    def __init__(
        self,
        output_settings: OutputSettings,
        stderr: IO = sys.stderr,
        stdout: IO = sys.stdout,
    ):
        self.settings = output_settings
        self.stderr = stderr
        self.stdout = stdout

        self.rule_matches: List[RuleMatch] = []
        self.debug_steps_by_rule: Dict[Rule, List[Dict[str, Any]]] = {}
        self.rules: FrozenSet[Rule] = frozenset()
        self.semgrep_core_errors: List[CoreException] = []
        self.semgrep_structured_errors: List[SemgrepError] = []
        self.error_set: Set[SemgrepError] = set()
        self.has_output = False

        self.final_error: Optional[Exception] = None

    def handle_semgrep_errors(self, errors: List[SemgrepError]) -> None:
        for err in errors:
            self.handle_semgrep_error(err)

    def handle_semgrep_error(self, error: SemgrepError) -> None:
        """
        Reports generic exceptions that extend SemgrepError
        """
        self.has_output = True
        if error not in self.error_set:
            self.semgrep_structured_errors.append(error)
            self.error_set.add(error)
            if self.settings.output_format == OutputFormat.TEXT:
                # Only show errors on stderr when not using format
                # that includes errors
                logger.error(str(error))

    def handle_semgrep_core_output(
        self,
        rule_matches_by_rule: Dict[Rule, List[RuleMatch]],
        debug_steps_by_rule: Dict[Rule, List[Dict[str, Any]]],
    ) -> None:
        self.has_output = True
        self.rules = self.rules.union(rule_matches_by_rule.keys())
        self.rule_matches += [
            match
            for matches_of_one_rule in rule_matches_by_rule.values()
            for match in matches_of_one_rule
        ]

        self.debug_steps_by_rule.update(debug_steps_by_rule)

    def handle_unhandled_exception(self, ex: Exception) -> None:
        """
        This is called by the context manager upon an unhandled exception. If you want to record a final
        error & set the exit code, but keep executing to perform cleanup tasks, call this method.
        """

        if isinstance(ex, SemgrepError):
            self.handle_semgrep_error(ex)
        self.final_error = ex

    def final_raise(self, ex: Optional[Exception]) -> None:
        if ex is None:
            return
        if isinstance(ex, SemgrepError):
            if ex.level == Level.ERROR:  # nosem: r2c.registry.latest useless-if-body
                raise ex
            else:
                if self.settings.strict:
                    raise ex
                logger.info(
                    "Warnings exist. Run with `--strict` to turn warnings into errors.",
                )
        else:
            raise ex

    def close(self) -> None:
        """
        Close the output handler.

        This will write any output that hasn't been written so far. It returns
        the exit code of the program.
        """
        if self.has_output:
            output = self.build_output(
                self.settings.output_destination is None and self.stdout.isatty()
            )
            if output:
                print(output, file=self.stdout)

            if self.settings.output_destination:
                self.save_output(self.settings.output_destination, output)

        final_error = None
        if self.final_error:
            final_error = self.final_error
        elif self.rule_matches and self.settings.error_on_findings:
            # This exception won't be visiable to the user, we're just
            # using this to return a specific error code
            final_error = SemgrepError("", code=FINDINGS_EXIT_CODE)
        elif self.semgrep_structured_errors:
            final_error = self.semgrep_structured_errors[-1]
        self.final_raise(final_error)

    @classmethod
    def save_output(cls, destination: str, output: str) -> None:
        if is_url(destination):
            cls.post_output(destination, output)
        else:
            if Path(destination).is_absolute():
                save_path = Path(destination)
            else:
                base_path = config_resolver.get_base_path()
                save_path = base_path.joinpath(destination)
            # create the folders if not exists
            save_path.parent.mkdir(parents=True, exist_ok=True)
            with save_path.open(mode="w") as fout:
                fout.write(output)

    @classmethod
    def post_output(cls, output_url: str, output: str) -> None:
        import requests  # here for faster startup times

        logger.info(f"posting to {output_url}...")
        try:
            r = requests.post(output_url, data=output, timeout=10)
            logger.debug(f"posted to {output_url} and got status_code:{r.status_code}")
        except requests.exceptions.Timeout:
            raise SemgrepError(f"posting output to {output_url} timed out")

    def build_output(self, color_output: bool) -> str:
        output_format = self.settings.output_format
        debug_steps = None
        if output_format == OutputFormat.JSON_DEBUG:
            debug_steps = self.debug_steps_by_rule
        if output_format.is_json():
            return build_output_json(
                self.rule_matches,
                self.semgrep_core_errors,
                self.semgrep_structured_errors,
                debug_steps,
            )
        elif output_format == OutputFormat.SARIF:
            return build_sarif_output(self.rule_matches, self.rules)
        elif output_format == OutputFormat.TEXT:
            return "\n".join(build_normal_output(self.rule_matches, color_output))
        else:
            # https://github.com/python/mypy/issues/6366
            raise RuntimeError(
                f"Unhandled output format: {type(output_format).__name__}"
            )
