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
from typing import Tuple

import attr

from semgrep.error import LegacySpan
from semgrep.error import Level
from semgrep.error import SemgrepCoreError
from semgrep.rule import Rule
from semgrep.rule_match import CoreLocation
from semgrep.rule_match import RuleMatch
from semgrep.types import JsonObject
from semgrep.types import RuleId
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

CoreErrorMessage = NewType("CoreErrorMessage", str)
CoreErrorType = NewType("CoreErrorType", str)
SkipReason = NewType("SkipReason", str)
SkipDetails = NewType("SkipDetails", str)


@attr.s(auto_attribs=True, frozen=True)
class MetavarValue:
    start: CoreLocation
    end: CoreLocation

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "MetavarValue":
        start = CoreLocation.parse(raw_json["start"])
        end = CoreLocation.parse(raw_json["end"])
        return cls(start, end)


@attr.s(auto_attribs=True, frozen=True)
class CoreMetavars:
    metavars: Dict[str, MetavarValue]

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreMetavars":
        metavars: Dict[str, MetavarValue] = {}
        for key, value in raw_json.items():
            metavars[key] = MetavarValue.parse(value)
        return cls(metavars)

    def get(self, key: str) -> MetavarValue:
        return self.metavars[key]

    def keys(self) -> List[str]:
        return list(self.metavars.keys())


@attr.s(auto_attribs=True, frozen=True)
class CoreMatch:
    """
    Encapsulates finding returned by semgrep-core
    """

    rule_id: RuleId
    path: Path
    start: CoreLocation
    end: CoreLocation
    extra: Dict[str, Any]
    metavars: CoreMetavars

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreMatch":
        rule_id = RuleId(raw_json["rule_id"])
        location = raw_json["location"]
        path_str = location["path"]
        assert isinstance(path_str, str)
        path = Path(path_str)
        start = CoreLocation.parse(location["start"])
        end = CoreLocation.parse(location["end"])
        extra = raw_json.get("extra", {})
        metavars = CoreMetavars.parse(extra.get("metavars"))
        return cls(rule_id, path, start, end, extra, metavars)


@attr.s(auto_attribs=True, frozen=True)
class CoreError:
    """
    Encapsulates error object returned by semgrep-core
    and handles conversion into SemgrepCoreError class that rest of codebase understands.
    """

    error_type: CoreErrorType
    rule_id: Optional[RuleId]
    path: Path
    start: CoreLocation
    end: CoreLocation
    message: CoreErrorMessage
    level: Level
    spans: Optional[Tuple[LegacySpan, ...]]
    details: Optional[str]

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreError":
        error_type = CoreErrorType(raw_json["error_type"])
        raw_rule_id = raw_json.get("rule_id")
        rule_id = RuleId(raw_rule_id) if raw_rule_id else None
        location = raw_json["location"]
        path = Path(location["path"])
        start = CoreLocation.parse(location["start"])
        end = CoreLocation.parse(location["end"])
        _extra = raw_json.get("extra", {})
        message = CoreErrorMessage(raw_json.get("message", "<no error message>"))
        level_str = raw_json["severity"]
        if level_str.upper() == "WARNING":
            level_str = "WARN"
        level = Level[level_str.upper()]

        details = raw_json.get("details")

        # TODO legacy support for live editor pattern parse highlighting
        spans = None
        if "yaml_path" in raw_json:
            yaml_path = tuple(raw_json["yaml_path"])
            yaml_path = yaml_path[::-1]
            spans = tuple([LegacySpan(start, end, yaml_path)])  # type: ignore

        return cls(
            error_type, rule_id, path, start, end, message, level, spans, details
        )

    def is_timeout(self) -> bool:
        """
        Return if this error is a match timeout
        """
        return self.error_type == CoreErrorType("Timeout")

    def to_semgrep_error(self, rule_id: RuleId) -> SemgrepCoreError:
        # TODO benchmarking code relies on error code value right now
        if self.error_type == CoreErrorType("Syntax error"):
            code = 3
        else:
            code = 2

        # TODO https://github.com/returntocorp/semgrep/issues/3861
        reported_rule_id = self.rule_id
        if self.is_timeout() or self.error_type == CoreErrorType("Out of memory"):
            reported_rule_id = rule_id

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


@attr.s(auto_attribs=True, frozen=True)
class CoreSkipped:
    rule_id: Optional[RuleId]
    path: Path
    reason: SkipReason
    details: SkipDetails

    @classmethod
    def parse(cls, raw_json: JsonObject) -> "CoreSkipped":
        raw_rule_id = raw_json.get("rule_id")
        rule_id = RuleId(raw_rule_id) if raw_rule_id else None
        path = Path(raw_json["path"])
        reason = SkipReason(raw_json["reason"])
        details = SkipDetails(raw_json["details"])
        if raw_rule_id:
            rule_info = f"rule {raw_rule_id}"
        else:
            rule_info = "all rules"
        logger.verbose(f"skipped '{path}' [{rule_info}]: {reason}: {details}")
        return cls(rule_id, path, reason, details)


@attr.s(auto_attribs=True, frozen=True)
class CoreTargetTiming:
    rule_id: RuleId
    target: Path
    parse_time: float
    match_time: float
    run_time: float

    @classmethod
    def parse(cls, raw_json: JsonObject, rule_id: RuleId) -> "CoreTargetTiming":
        # rule_id = RuleId(raw_json["rule_id"])
        path = Path(raw_json["path"])
        parse_time = raw_json["parse_time"]
        match_time = raw_json["match_time"]
        run_time = raw_json["run_time"]
        return cls(rule_id, path, parse_time, match_time, run_time)


@attr.s(auto_attribs=True, frozen=True)
class CoreRuleParseTiming:
    rule_id: RuleId
    parse_time: float

    @classmethod
    def parse(cls, raw_json: JsonObject, rule_id: RuleId) -> "CoreRuleParseTiming":
        # rule_id = RuleId(raw_json["rule_id"])
        parse_time = float(raw_json["rule_parse_time"])
        return cls(rule_id, parse_time)


@attr.s(auto_attribs=True, frozen=True)
class CoreTiming:
    target_timings: List[CoreTargetTiming]
    rule_parse_timings: List[CoreRuleParseTiming]

    @classmethod
    def parse(cls, raw_json: JsonObject, rule_id: RuleId) -> "CoreTiming":
        if not raw_json:
            return cls([], [])

        target_timings = raw_json.get("targets", [])
        parsed_target_timings = []
        for obj in target_timings:
            parsed_target_timings.append(CoreTargetTiming.parse(obj, rule_id))

        parsed_rule_parse_timings = [CoreRuleParseTiming.parse(raw_json, rule_id)]

        return cls(parsed_target_timings, parsed_rule_parse_timings)


@attr.s(auto_attribs=True)
class CoreOutput:
    """
    Parses output of semgrep-core
    """

    matches: List[CoreMatch]
    errors: List[CoreError]
    skipped: List[CoreSkipped]
    timing: CoreTiming

    @classmethod
    def parse(cls, raw_json: JsonObject, rule_id: RuleId) -> "CoreOutput":
        parsed_errors = []
        errors = raw_json["errors"]
        for error in errors:
            parsed_errors.append(CoreError.parse(error))

        parsed_matches = []
        matches = raw_json["matches"]
        for match in matches:
            parsed_matches.append(CoreMatch.parse(match))

        parsed_skipped = []
        skipped = raw_json["skipped"]
        for skip in skipped:
            parsed_skipped.append(CoreSkipped.parse(skip))

        timings = raw_json.get("time", {})
        parsed_timings = CoreTiming.parse(
            timings, rule_id
        )  # For now assume only one rule run at a time

        return cls(parsed_matches, parsed_errors, parsed_skipped, parsed_timings)

    def rule_matches(self, rule: Rule) -> List[RuleMatch]:
        """
        Convert core_match objects into RuleMatch objects that the rest of the codebase
        interacts with.

        For now assumes that all matches encapsulated by this object are from the same rulee
        """
        # This will remove matches that have the same range but different
        # metavariable bindings, choosing the last one in the list. We want the
        # last because if there multiple possible bindings, they will be returned
        # by semgrep-core from largest range to smallest. For an example, see
        # tests/e2e/test_message_interpolation.py::test_message_interpolation;
        # specifically, the multi-pattern-inside test
        #
        # Another option is to not dedup, since Semgrep.ml now does its own deduping
        # otherwise, and surface both matches
        def dedup(outputs: List[RuleMatch]) -> List[RuleMatch]:
            return list({uniq_id(r): r for r in reversed(outputs)}.values())[::-1]

        def uniq_id(
            r: RuleMatch,
        ) -> Tuple[str, Path, int, int, str]:
            start = r.start
            end = r.end
            return (
                r.id,
                r.path,
                start.offset,
                end.offset,
                r.message,
            )

        def interpolate(text: str, metavariables: Dict[str, str]) -> str:
            """Interpolates a string with the metavariables contained in it, returning a new string"""

            # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
            for metavariable in sorted(metavariables.keys(), key=len, reverse=True):
                text = text.replace(metavariable, metavariables[metavariable])

            return text

        def read_metavariables(match: CoreMatch) -> Dict[str, str]:
            result = {}

            # open path and ignore non-utf8 bytes. https://stackoverflow.com/a/56441652
            with open(match.path, errors="replace") as fd:
                for metavariable in match.metavars.keys():
                    metavariable_data = match.metavars.get(metavariable)
                    # Offsets are start inclusive and end exclusive
                    start_offset = metavariable_data.start.offset
                    end_offset = metavariable_data.end.offset
                    length = end_offset - start_offset

                    fd.seek(start_offset)
                    result[metavariable] = fd.read(length)

            return result

        def convert_to_rule_match(match: CoreMatch, rule: Rule) -> RuleMatch:
            metavariables = read_metavariables(match)
            message = interpolate(rule.message, metavariables)
            fix = interpolate(rule.fix, metavariables) if rule.fix else None

            rule_match = RuleMatch(
                rule.id,
                message=message,
                metadata=rule.metadata,
                severity=rule.severity,
                fix=fix,
                fix_regex=rule.fix_regex,
                path=match.path,
                start=match.start,
                end=match.end,
                extra=match.extra,
                lines_cache={},
            )
            return rule_match

        findings = []
        for match in self.matches:
            rule_match = convert_to_rule_match(match, rule)
            findings.append(rule_match)

        # Sort results so as to guarantee the same results across different
        # runs. Results may arrive in a different order due to parallelism
        # (-j option).
        findings = sorted(
            findings, key=lambda rule_match: [rule_match.path, rule_match.start.offset]
        )
        findings = dedup(findings)
        return findings
