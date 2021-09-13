from dataclasses import dataclass
from re import sub
from typing import List
from typing import Sequence
from typing import Tuple

import attr

from semgrep.constants import COMMA_SEPARATED_LIST_RE
from semgrep.constants import NOSEM_INLINE_RE
from semgrep.error import Level
from semgrep.error import SemgrepError
from semgrep.output import logger
from semgrep.output import OutputHandler
from semgrep.rule_match import RuleMatch
from semgrep.types import RuleMatchMap


@dataclass
class IgnoreResults:
    num_matches: int
    matches: RuleMatchMap
    errors: Sequence[SemgrepError]


def process_ignores(
    rule_matches: RuleMatchMap,
    output_handler: OutputHandler,
    *,
    strict: bool,
    disable_nosem: bool,
) -> IgnoreResults:
    filtered = {}
    nosem_errors: List[SemgrepError] = []
    for rule, matches in rule_matches.items():
        evolved_matches = []
        for match in matches:
            ignored, returned_errors = _rule_match_nosem(match, strict)
            evolved_matches.append(attr.evolve(match, is_ignored=ignored))
            nosem_errors.extend(returned_errors)
        filtered[rule] = evolved_matches

    num_findings_nosem = 0
    if not disable_nosem:
        if not output_handler.formatter.keep_ignores():
            filtered = {
                rule: [m for m in matches if not m._is_ignored]
                for rule, matches in filtered.items()
            }
        num_findings_nosem = sum(
            1 for rule, matches in filtered.items() for m in matches if m._is_ignored
        )

    return IgnoreResults(num_findings_nosem, filtered, nosem_errors)


def _rule_match_nosem(
    rule_match: RuleMatch, strict: bool
) -> Tuple[bool, Sequence[SemgrepError]]:
    if not rule_match.lines:
        return False, []

    # Only consider the first line of a match. This will keep consistent
    # behavior on where we expect a 'nosem' comment to exist. If we allow these
    # comments on any line of a match it will get confusing as to what finding
    # the 'nosem' is referring to.
    re_match = NOSEM_INLINE_RE.search(rule_match.lines[0])
    if re_match is None:
        return False, []

    ids_str = re_match.groupdict()["ids"]
    if ids_str is None:
        logger.verbose(
            f"found 'nosem' comment, skipping rule '{rule_match.id}' on line {rule_match.start['line']}"
        )
        return True, []

    # Strip quotes to allow for use of nosem as an HTML attribute inside tags.
    # HTML comments inside tags are not allowed by the spec.
    pattern_ids = {
        pattern_id.strip().strip("\"'")
        for pattern_id in COMMA_SEPARATED_LIST_RE.split(ids_str)
        if pattern_id.strip()
    }

    # Filter out ids that are not alphanum+dashes+underscores+periods.
    # This removes trailing symbols from comments, such as HTML comments `-->`
    # or C-like multiline comments `*/`.
    pattern_ids = set(filter(lambda x: not sub(r"[\w\-\.]+", "", x), pattern_ids))

    errors = []
    result = False
    for pattern_id in pattern_ids:
        if rule_match.id == pattern_id:
            logger.verbose(
                f"found 'nosem' comment with id '{pattern_id}', skipping rule '{rule_match.id}' on line {rule_match.start['line']}"
            )
            result = result or True
        else:
            message = f"found 'nosem' comment with id '{pattern_id}', but no corresponding rule trying '{rule_match.id}'"
            if strict:
                errors.append(SemgrepError(message, level=Level.WARN))
            else:
                logger.verbose(message)

    return result, errors
