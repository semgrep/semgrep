"""
Handles ignoring Semgrep findings via inline code comments

Currently supports ignoring a finding on a single line by adding a
`# nosemgrep:ruleid` comment (or `// nosemgrep:ruleid`).

To use, create a RuleMatchMap, then pass it to process_ignores().
"""
from re import sub
from typing import List
from typing import Sequence
from typing import Tuple

import attr

from semgrep.constants import COMMA_SEPARATED_LIST_RE
from semgrep.constants import NOSEM_INLINE_RE
from semgrep.error import Level
from semgrep.error import SemgrepError
from semgrep.rule_match import RuleMatch
from semgrep.rule_match_map import RuleMatchMap
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def process_ignores(
    rule_matches_by_rule: RuleMatchMap,
    keep_ignored: bool,
    *,
    strict: bool,
) -> Tuple[RuleMatchMap, Sequence[SemgrepError], int]:
    """
    Converts a mapping of findings to a mapping of findings that
    will be shown to the caller.

    :param rule_matches_by_rule: The input findings (typically from a Semgrep call)
    :param keep_ignored: if true will keep nosem findings in returned object, otherwise removes them
    :param strict: The value of the --strict flag (affects error return)
    :return:
    - RuleMatchMap: dict from rule to list of findings. Findings have is_ignored
        set to true if there was matching nosem comment found for it.
        If keep_ignored set to true, will keep all findings that have is_ignored: True
        otherwise removes them in the return object
    - list of semgrep errors when dealing with nosem:
        i.e. a nosem without associated finding or nosem id not matching finding
    - number of findings with is_ignored set to true
    """
    filtered = {}
    nosem_errors: List[SemgrepError] = []
    for rule, matches in rule_matches_by_rule.items():
        evolved_matches = []
        for match in matches:
            ignored, returned_errors = _rule_match_nosem(match, strict)
            evolved_matches.append(attr.evolve(match, is_ignored=ignored))
            nosem_errors.extend(returned_errors)
        filtered[rule] = evolved_matches

    if not keep_ignored:
        filtered = {
            rule: [m for m in matches if not m._is_ignored]
            for rule, matches in filtered.items()
        }

    num_findings_nosem = sum(
        1 for rule, matches in filtered.items() for m in matches if m._is_ignored
    )

    return filtered, nosem_errors, num_findings_nosem


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
            f"found 'nosem' comment, skipping rule '{rule_match.id}' on line {rule_match.start.line}"
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
                f"found 'nosem' comment with id '{pattern_id}', skipping rule '{rule_match.id}' on line {rule_match.start.line}"
            )
            result = result or True
        else:
            message = f"found 'nosem' comment with id '{pattern_id}', but no corresponding rule trying '{rule_match.id}'"
            if strict:
                errors.append(SemgrepError(message, level=Level.WARN))
            else:
                logger.verbose(message)

    return result, errors
