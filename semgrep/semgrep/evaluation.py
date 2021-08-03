from typing import Dict
from typing import List
from typing import Optional
from typing import Set

from semgrep.pattern_match import PatternMatch
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import Range
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def compare_propagated_metavariable(
    _range: Range,
    pattern_match: PatternMatch,
    metavariable: str,
) -> bool:

    return (
        metavariable in _range.propagated_metavariables
        and metavariable in pattern_match.metavariable_uids
        and _range.propagated_metavariables[metavariable]
        == pattern_match.metavariable_uids[metavariable]
    )


def create_output(
    rule: Rule,
    pattern_matches: List[PatternMatch],
    valid_ranges_to_output: Optional[Set[Range]] = None,
) -> List[RuleMatch]:
    output = []

    if valid_ranges_to_output is None:
        valid_ranges_to_output = {
            pattern_match.range for pattern_match in pattern_matches
        }

    propagated_metavariable_lookup = {
        _range: {
            metavariable: pm.get_metavariable_value(metavariable)
            for pm in pattern_matches
            for metavariable in _range.propagated_metavariables
            if compare_propagated_metavariable(_range, pm, metavariable)
        }
        for _range in valid_ranges_to_output
    }

    for pattern_match in pattern_matches:
        if pattern_match.range in valid_ranges_to_output:
            propagated_metavariables = propagated_metavariable_lookup[
                pattern_match.range
            ]
            message = interpolate_string_with_metavariables(
                rule.message, pattern_match, propagated_metavariables
            )
            fix = (
                interpolate_string_with_metavariables(
                    rule.fix, pattern_match, propagated_metavariables
                )
                if rule.fix
                else None
            )
            rule_match = RuleMatch.from_pattern_match(
                rule.id,
                pattern_match,
                message=message,
                metadata=rule.metadata,
                severity=rule.severity,
                fix=fix,
                fix_regex=rule.fix_regex,
            )
            output.append(rule_match)

    return sorted(output, key=lambda rule_match: rule_match._pattern_match.range.start)


def interpolate_string_with_metavariables(
    text: str, pattern_match: PatternMatch, propagated_metavariables: Dict[str, str]
) -> str:
    """Interpolates a string with the metavariables contained in it, returning a new string"""

    # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
    for metavariable in sorted(pattern_match.metavariables, key=len, reverse=True):
        text = text.replace(
            metavariable, pattern_match.get_metavariable_value(metavariable)
        )

    # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
    for metavariable in sorted(propagated_metavariables.keys(), key=len, reverse=True):
        text = text.replace(metavariable, propagated_metavariables[metavariable])

    return text
