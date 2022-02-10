"""
This file encapsulates classes necessary in parsing semgrep-core json output into a typed object

The objects in this class should expose functionality that returns objects that the rest of the codebase
interacts with (e.g. the rest of the codebase should be interacting with RuleMatch objects instead of CoreMatch
and SemgrepCoreError instead of CoreError objects).

The precise type of the response from semgrep-core is specified in
Semgrep_core_response.atd, currently at:
https://github.com/returntocorp/semgrep/blob/develop/semgrep-core/src/core-response/Semgrep_core_response.atd
"""

from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import NewType
from attrs import define, field
from pathlib import Path
from semgrep.rule import Rule
import semgrep.types as types
import semgrep.error as error
import semgrep.types as types
import semgrep.rule_match as rule_match

@define(frozen=True)
class MetavarValue:
    start: rule_match.CoreLocation
    end: rule_match.CoreLocation

@define(frozen=True)
class CoreMetavars:
    metavars: Dict[str, MetavarValue]

@define(frozen=True)
class CoreMatch:
    """
    Encapsulates finding returned by semgrep-core
    """

    rule: Rule
    path: Path
    start: rule_match.CoreLocation
    end: rule_match.CoreLocation
    extra: Dict[str, Any]
    metavars: CoreMetavars

@define(frozen=True)
class CoreRuleTiming:  # For a given target
    rule: Rule
    parse_time: float
    match_time: float

@define(frozen=True)
class CoreTargetTiming:
    target: Path
    per_rule_timings: List[CoreRuleTiming]
    run_time: float

CoreRulesParseTime = NewType("CoreRulesParseTime", float)

@define(frozen=True)
class CoreTiming:
    rules: List[Rule]
    target_timings: List[CoreTargetTiming]
    rules_parse_time: CoreRulesParseTime

CoreErrorType = NewType("CoreErrorType", str)
CoreErrorMessage = NewType("CoreErrorMessage", str)

@define(frozen=True)
class CoreError:
    """
    Encapsulates error object returned by semgrep-core
    and handles conversion into SemgrepCoreError class that rest of codebase understands.
    """

    error_type: CoreErrorType
    rule_id: Optional[types.RuleId]
    path: Path
    start: rule_match.CoreLocation
    end: rule_match.CoreLocation
    message: CoreErrorMessage
    level: error.Level
    spans: Optional[Tuple[error.LegacySpan, ...]]
    details: Optional[str]

SkipReason = NewType("SkipReason", str)
SkipDetails = NewType("SkipDetails", str)

@define(frozen=True)
class CoreSkipped:
    rule_id: Optional[types.RuleId]
    path: Path
    reason: SkipReason
    details: SkipDetails

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
    def parse(
        cls, raw_json: types.JsonObject, rule_id: types.RuleId
    ) -> "CoreOutput": ...
