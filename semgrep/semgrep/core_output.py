"""
This file encapsulates classes necessary in parsing semgrep-core
json output into a typed object

The precise type of the response from semgrep-core is specified in
https://github.com/returntocorp/semgrep/blob/develop/interfaces/Output_from_core.atd
"""
from pathlib import Path
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from attrs import define
from attrs import frozen

import semgrep.output_from_core as core
from semgrep.error import LegacySpan
from semgrep.error import Level
from semgrep.error import SemgrepCoreError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchSet
from semgrep.types import JsonObject
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


@frozen
class CoreError:
    """
    Encapsulates error object returned by semgrep-core
    and handles conversion into SemgrepCoreError class that rest of codebase understands.
    """

    error_type: str
    rule_id: Optional[core.RuleId]
    location: core.Location
    message: str
    details: Optional[str]
    severity: core.Severity
    yaml_path: Optional[List[str]]

    @classmethod
    def make(cls, error: core.Error) -> "CoreError":
        error_type = error.error_type
        rule_id = error.rule_id
        location = error.location
        message = error.message
        details = error.details
        severity = error.severity
        yaml_path = error.yaml_path

        return cls(error_type, rule_id, location, message, details, severity, yaml_path)

    def is_timeout(self) -> bool:
        """
        Return if this error is a match timeout
        """
        return self.error_type == "Timeout"

    def to_semgrep_error(self) -> SemgrepCoreError:
        reported_rule_id = self.rule_id

        # Hackily convert the level string to Semgrep expectations
        level_str = self.severity.kind
        if level_str.upper() == "WARNING":
            level_str = "WARN"
        if level_str.upper() == "ERROR_":
            level_str = "ERROR"
        level = Level[level_str.upper()]

        spans: Optional[Tuple[LegacySpan, ...]] = None
        if self.yaml_path:
            yaml_path = tuple(self.yaml_path[::-1])
            spans = tuple([LegacySpan(self.location.start, self.location.end, yaml_path)])  # type: ignore

        # TODO benchmarking code relies on error code value right now
        # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
        if self.error_type == "Syntax error" or self.error_type == "Lexical error":
            code = 3
            reported_rule_id = None  # Rule id not important for parse errors
        else:
            code = 2

        return SemgrepCoreError(
            code,
            level,
            self.error_type,
            reported_rule_id,
            Path(self.location.path),
            self.location.start,
            self.location.end,
            self.message,
            spans,
            self.details,
        )


@define
class CoreOutput:
    """
    Parses output of semgrep-core
    """

    matches: List[core.Match]
    errors: List[CoreError]
    skipped: List[core.SkippedTarget]
    timing: Optional[core.Time]

    @classmethod
    def parse(cls, rules: List[Rule], raw_json: JsonObject) -> "CoreOutput":

        match_results = core.MatchResults.from_json(raw_json)

        parsed_errors = [CoreError.make(error) for error in match_results.errors]
        matches = match_results.matches
        for skip in match_results.skipped_targets:
            if skip.rule_id:
                rule_info = f"rule {skip.rule_id}"
            else:
                rule_info = "all rules"
            logger.verbose(
                f"skipped '{skip.path}' [{rule_info}]: {skip.reason}: {skip.details}"
            )

        skipped = match_results.skipped_targets
        timings = match_results.time

        return cls(matches, parsed_errors, skipped, timings)

    def rule_matches(self, rules: List[Rule]) -> Dict[Rule, List[RuleMatch]]:
        """
        Convert core_match objects into RuleMatch objects that the rest of the codebase
        interacts with.

        For now assumes that all matches encapsulated by this object are from the same rulee
        """
        rule_table = {rule.id: rule for rule in rules}

        def interpolate(text: str, metavariables: Dict[str, str]) -> str:
            """Interpolates a string with the metavariables contained in it, returning a new string"""

            # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
            for metavariable in sorted(metavariables.keys(), key=len, reverse=True):
                text = text.replace(metavariable, metavariables[metavariable])

            return text

        def read_metavariables(match: core.Match) -> Dict[str, str]:
            result = {}

            # open path and ignore non-utf8 bytes. https://stackoverflow.com/a/56441652
            with open(match.location.path, errors="replace") as fd:
                for metavariable, metavariable_data in match.extra.metavars.items():
                    # Offsets are start inclusive and end exclusive
                    start_offset = metavariable_data.start.offset
                    end_offset = metavariable_data.end.offset
                    length = end_offset - start_offset

                    fd.seek(start_offset)
                    result[metavariable] = fd.read(length)

            return result

        def convert_to_rule_match(match: core.Match) -> RuleMatch:
            rule = rule_table[match.rule_id.value]
            metavariables = read_metavariables(match)
            message = interpolate(rule.message, metavariables)
            fix = interpolate(rule.fix, metavariables) if rule.fix else None

            return RuleMatch(
                match.rule_id.value,
                message=message,
                metadata=rule.metadata,
                severity=rule.severity,
                fix=fix,
                fix_regex=rule.fix_regex,
                path=Path(match.location.path),
                start=match.location.start,
                end=match.location.end,
                extra=match.extra.to_json(),
            )

        # TODO: Dict[core.RuleId, RuleMatchSet]
        findings: Dict[Rule, RuleMatchSet] = {rule: RuleMatchSet() for rule in rules}
        seen_cli_unique_keys: Set[Tuple] = set()
        for match in self.matches:
            rule = rule_table[match.rule_id.value]
            rule_match = convert_to_rule_match(match)
            if rule_match.cli_unique_key in seen_cli_unique_keys:
                continue
            seen_cli_unique_keys.add(rule_match.cli_unique_key)
            findings[rule].add(rule_match)

        # Sort results so as to guarantee the same results across different
        # runs. Results may arrive in a different order due to parallelism
        # (-j option).
        return {rule: sorted(matches) for rule, matches in findings.items()}
