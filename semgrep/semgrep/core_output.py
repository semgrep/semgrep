"""
This file encapsulates classes necessary in parsing semgrep-core json output into a typed object

The objects in this class should expose functionality that returns objects that the rest of the codebase
interacts with (e.g. the rest of the codebase should be interacting with RuleMatch objects instead of CoreMatch
and SemgrepCoreError instead of CoreError objects).

The precise type of the response from semgrep-core is specified in
Semgrep_core_response.atd, currently at:
https://github.com/returntocorp/semgrep/blob/develop/semgrep-core/src/core-response/Semgrep_core_response.atd
"""
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import NewType
from typing import Optional
from typing import Set
from typing import Tuple

from attrs import define
from attrs import frozen

import semgrep.output_from_core as core
from semgrep.error import LegacySpan
from semgrep.error import Level
from semgrep.error import SemgrepCoreError
from semgrep.output_from_core import MetavarValue
from semgrep.output_from_core import SkipReason
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchSet
from semgrep.types import JsonObject
from semgrep.types import RuleId
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

CoreErrorMessage = NewType("CoreErrorMessage", str)
CoreErrorType = NewType("CoreErrorType", str)
SkipDetails = NewType("SkipDetails", str)

CoreRulesParseTime = NewType("CoreRulesParseTime", float)


@frozen
class CoreMatch:
    """
    Encapsulates finding returned by semgrep-core
    """

    rule: Rule
    path: Path
    start: core.Position
    end: core.Position
    extra: Dict[str, Any]
    metavars: Dict[str, MetavarValue]

    @classmethod
    def make(cls, rule_table: Dict[str, Rule], match: core.Match) -> "CoreMatch":
        rule = rule_table[match.rule_id.value]
        path = Path(match.location.path)
        start = match.location.start
        end = match.location.end
        extra = match.extra.to_json()
        metavars = match.extra.metavars
        return cls(rule, path, start, end, extra, metavars)


@frozen
class CoreError:
    """
    Encapsulates error object returned by semgrep-core
    and handles conversion into SemgrepCoreError class that rest of codebase understands.
    """

    error_type: CoreErrorType
    rule_id: Optional[RuleId]
    path: Path
    start: core.Position
    end: core.Position
    message: CoreErrorMessage
    level: Level
    spans: Optional[Tuple[LegacySpan, ...]]
    details: Optional[str]

    @classmethod
    def make(cls, error: core.Error) -> "CoreError":
        error_type = CoreErrorType(error.error_type)
        rule_id = RuleId(error.rule_id.value) if error.rule_id else None
        path = Path(error.location.path)
        start = error.location.start
        end = error.location.end
        message = CoreErrorMessage(error.message)

        # Hackily convert the level string to Semgrep expectations
        level_str = error.severity.kind
        if level_str.upper() == "WARNING":
            level_str = "WARN"
        if level_str.upper() == "ERROR_":
            level_str = "ERROR"
        level = Level[level_str.upper()]
        details = error.details

        spans = None
        if error.yaml_path:
            yaml_path = tuple(error.yaml_path[::-1])
            spans = tuple([LegacySpan(start, end, yaml_path)])  # type: ignore
        return cls(
            error_type, rule_id, path, start, end, message, level, spans, details
        )

    def is_timeout(self) -> bool:
        """
        Return if this error is a match timeout
        """
        return self.error_type == CoreErrorType("Timeout")

    def to_semgrep_error(self) -> SemgrepCoreError:
        reported_rule_id = self.rule_id

        # TODO benchmarking code relies on error code value right now
        # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
        if self.error_type == CoreErrorType(
            "Syntax error"
        ) or self.error_type == CoreErrorType("Lexical error"):
            code = 3
            reported_rule_id = None  # Rule id not important for parse errors
        else:
            code = 2

        return SemgrepCoreError(
            code,
            self.level,
            self.error_type,
            reported_rule_id,
            self.path,
            self.start,
            self.end,
            self.message,
            self.spans,
            self.details,
        )


@frozen
class CoreSkipped:
    rule_id: Optional[RuleId]
    path: Path
    reason: SkipReason
    details: SkipDetails

    @classmethod
    def make(cls, skipped: core.SkippedTarget) -> "CoreSkipped":
        rule_id = RuleId(skipped.rule_id.value) if skipped.rule_id else None
        path = Path(skipped.path)
        reason = skipped.reason
        details = SkipDetails(skipped.details)

        if rule_id:
            rule_info = f"rule {rule_id}"
        else:
            rule_info = "all rules"
        logger.verbose(f"skipped '{str(path)}' [{rule_info}]: {reason}: {details}")
        return cls(rule_id, path, reason, details)


@frozen
class CoreRuleTiming:  # For a given target
    rule: Rule
    parse_time: float
    match_time: float

    @classmethod
    def make(
        cls, rule_table: Dict[str, Rule], rule_time: core.RuleTimes
    ) -> "CoreRuleTiming":
        rule = rule_table[rule_time.rule_id]
        parse_time = rule_time.parse_time
        match_time = rule_time.match_time
        return cls(rule, parse_time, match_time)


@frozen
class CoreTargetTiming:
    target: Path
    per_rule_timings: List[CoreRuleTiming]
    run_time: float

    @classmethod
    def make(
        cls, rule_table: Dict[str, Rule], target_time: core.TargetTime
    ) -> "CoreTargetTiming":
        target = Path(target_time.path)
        per_rule_timings = [
            CoreRuleTiming.make(rule_table, rule_time)
            for rule_time in target_time.rule_times
        ]
        run_time = target_time.run_time
        return cls(target, per_rule_timings, run_time)


@frozen
class CoreTiming:
    rules: List[Rule]
    target_timings: List[CoreTargetTiming]
    rules_parse_time: CoreRulesParseTime

    @classmethod
    def make(
        cls, rule_table: Dict[str, Rule], time: Optional[core.Time]
    ) -> "CoreTiming":
        if not time:
            return cls([], [], CoreRulesParseTime(0.0))

        rules = [rule_table[rule] for rule in time.rules]
        target_timings = [
            CoreTargetTiming.make(rule_table, target) for target in time.targets
        ]
        rules_parse_time = CoreRulesParseTime(
            time.rules_parse_time if time.rules_parse_time else 0.0
        )
        return cls(rules, target_timings, rules_parse_time)


@define
class CoreOutput:
    """
    Parses output of semgrep-core
    """

    matches: List[CoreMatch]
    errors: List[CoreError]
    skipped: List[CoreSkipped]
    timing: CoreTiming

    @classmethod
    def parse(cls, rules: List[Rule], raw_json: JsonObject) -> "CoreOutput":
        rule_table = {rule.id: rule for rule in rules}

        match_results = core.MatchResults.from_json(raw_json)

        parsed_errors = [CoreError.make(error) for error in match_results.errors]
        parsed_matches = [
            CoreMatch.make(rule_table, match) for match in match_results.matches
        ]
        parsed_skipped = [
            CoreSkipped.make(skip) for skip in match_results.skipped_targets
        ]
        parsed_timings = CoreTiming.make(rule_table, match_results.time)

        return cls(parsed_matches, parsed_errors, parsed_skipped, parsed_timings)

    def rule_matches(self, rules: List[Rule]) -> Dict[Rule, List[RuleMatch]]:
        """
        Convert core_match objects into RuleMatch objects that the rest of the codebase
        interacts with.

        For now assumes that all matches encapsulated by this object are from the same rulee
        """

        def interpolate(
            text: str, metavariables: Dict[str, str], propgated_values: Dict[str, str]
        ) -> str:
            """Interpolates a string with the metavariables contained in it, returning a new string"""

            # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
            for metavariable in sorted(metavariables.keys(), key=len, reverse=True):
                text = text.replace(metavariable, metavariables[metavariable])
                text = text.replace("$" + metavariable, propgated_values[metavariable])

            return text

        def read_metavariables(
            match: CoreMatch,
        ) -> Tuple[Dict[str, str], Dict[str, str]]:
            matched_values = {}
            propagated_values = {}

            # open path and ignore non-utf8 bytes. https://stackoverflow.com/a/56441652
            with open(match.path, errors="replace") as fd:
                for metavariable, metavariable_data in match.metavars.items():
                    # Offsets are start inclusive and end exclusive
                    start_offset = metavariable_data.start.offset
                    end_offset = metavariable_data.end.offset
                    length = end_offset - start_offset

                    fd.seek(start_offset)
                    matched_value = fd.read(length)

                    # Use propagated value
                    if metavariable_data.propagated_value:
                        propagated_value = (
                            metavariable_data.propagated_value.svalue_abstract_content
                        )
                    else:
                        propagated_value = matched_value

                    matched_values[metavariable] = matched_value
                    propagated_values[metavariable] = propagated_value

            return matched_values, propagated_values

        def convert_to_rule_match(match: CoreMatch) -> RuleMatch:
            rule = match.rule
            matched_values, propagated_values = read_metavariables(match)
            message = interpolate(rule.message, matched_values, propagated_values)
            fix = (
                interpolate(rule.fix, matched_values, propagated_values)
                if rule.fix
                else None
            )

            return RuleMatch(
                rule._id,
                message=message,
                metadata=rule.metadata,
                severity=rule.severity,
                fix=fix,
                fix_regex=rule.fix_regex,
                path=match.path,
                start=match.start,
                end=match.end,
                extra=match.extra,
            )

        findings: Dict[Rule, RuleMatchSet] = {rule: RuleMatchSet() for rule in rules}
        seen_cli_unique_keys: Set[Tuple] = set()
        for match in self.matches:
            rule_match = convert_to_rule_match(match)
            if rule_match.cli_unique_key in seen_cli_unique_keys:
                continue
            seen_cli_unique_keys.add(rule_match.cli_unique_key)
            findings[match.rule].add(rule_match)

        # Sort results so as to guarantee the same results across different
        # runs. Results may arrive in a different order due to parallelism
        # (-j option).
        return {rule: sorted(matches) for rule, matches in findings.items()}
