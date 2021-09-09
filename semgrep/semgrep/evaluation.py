from typing import List

from semgrep.pattern_match import PatternMatch
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def create_output(
    rule: Rule,
    pattern_matches: List[PatternMatch],
) -> List[RuleMatch]:
    output = []
    for pattern_match in pattern_matches:
        message = interpolate_string_with_metavariables(rule.message, pattern_match)
        fix = (
            interpolate_string_with_metavariables(rule.fix, pattern_match)
            if rule.fix
            else None
        )
        rule_match = RuleMatch(
            rule.id,
            message=message,
            metadata=rule.metadata,
            severity=rule.severity,
            fix=fix,
            fix_regex=rule.fix_regex,
            path=pattern_match.path,
            start=pattern_match.start,
            end=pattern_match.end,
            extra=pattern_match.extra,
            lines_cache={},
        )
        output.append(rule_match)

    return sorted(output, key=lambda rule_match: rule_match.start.get("offset", 0))


def interpolate_string_with_metavariables(
    text: str, pattern_match: PatternMatch
) -> str:
    """Interpolates a string with the metavariables contained in it, returning a new string"""

    # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
    for metavariable in sorted(pattern_match.metavariables, key=len, reverse=True):
        text = text.replace(
            metavariable, pattern_match.get_metavariable_value(metavariable)
        )

    return text
