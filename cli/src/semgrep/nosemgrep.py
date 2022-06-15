"""
Handles ignoring Semgrep findings via inline code comments

Currently supports ignoring a finding on a single line by adding a
`# nosemgrep:ruleid` comment (or `// nosemgrep:ruleid`).

To use, create a RuleMatchMap, then pass it to process_ignores().

Coupling: semgrep-action uses a regexp to strip nosemgrep comments so as
to normalize the code and not be sensitive to the addition or removal
of nosemgrep comments.
See https://github.com/returntocorp/semgrep-action/blob/develop/src/semgrep_agent/findings.py
and check that it's compatible with any change we're making here.
"""
from re import sub
from typing import List
from typing import Sequence
from typing import Tuple

from attrs import evolve
from boltons.iterutils import partition

from semgrep.constants import COMMA_SEPARATED_LIST_RE
from semgrep.constants import NOSEM_INLINE_RE
from semgrep.constants import NOSEM_PREVIOUS_LINE_RE
from semgrep.error import Level
from semgrep.error import SemgrepError
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.types import FilteredMatches
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def process_ignores(
    rule_matches_by_rule: RuleMatchMap,
    *,
    keep_ignored: bool,
    strict: bool,
) -> Tuple[FilteredMatches, Sequence[SemgrepError]]:
    """
    Converts a mapping of findings to a mapping of findings that
    will be shown to the caller.

    :param rule_matches_by_rule: The input findings (typically from a Semgrep call)
    :param keep_ignored: if true will keep nosem findings in returned object, otherwise removes them
    :param strict: The value of the --strict flag (affects error return)
    :return:
    - FilteredMatches: dicts from rule to list of findings. Findings have is_ignored
        set to true if there was matching nosem comment found for it.
        If keep_ignored set to true, will keep all findings that have is_ignored: True
        in the .kept attribute, otherwise moves them to .removed
    - list of semgrep errors when dealing with nosem:
        i.e. a nosem without associated finding or nosem id not matching finding
    """
    result = FilteredMatches(rule_matches_by_rule)
    errors: List[SemgrepError] = []
    for rule, matches in rule_matches_by_rule.items():
        evolved_matches = []
        for match in matches:
            ignored, returned_errors = _rule_match_nosem(match, strict)
            evolved_matches.append(evolve(match, is_ignored=ignored))
            errors.extend(returned_errors)
        result.kept[rule], result.removed[rule] = partition(
            evolved_matches, lambda match: keep_ignored or not match.is_ignored
        )

    return result, errors


def _rule_match_nosem(
    rule_match: RuleMatch, strict: bool
) -> Tuple[bool, Sequence[SemgrepError]]:
    if not rule_match.lines:
        return False, []

    ids: List[str] = []

    # Only consider the first line of a match. This will keep consistent
    # behavior on where we expect a 'nosem' comment to exist. If we allow these
    # comments on any line of a match it will get confusing as to what finding
    # the 'nosem' is referring to.
    lines_re_match = NOSEM_INLINE_RE.search(rule_match.lines[0])
    if lines_re_match:
        lines_ids_str = lines_re_match.groupdict()["ids"]
        if lines_ids_str:
            ids = ids + COMMA_SEPARATED_LIST_RE.split(lines_ids_str)

    # Same thing, but inspect the line just before the match.
    # This involves a different regexp. Also captures rule names as 'ids'.
    prev_line_re_match = NOSEM_PREVIOUS_LINE_RE.search(rule_match.previous_line)
    if prev_line_re_match:
        prev_line_ids_str = prev_line_re_match.groupdict()["ids"]
        if prev_line_ids_str:
            ids = ids + COMMA_SEPARATED_LIST_RE.split(prev_line_ids_str)

    if lines_re_match is None and prev_line_re_match is None:
        return False, []

    if not ids:
        logger.verbose(
            f"found 'nosem' comment, skipping rule '{rule_match.rule_id}' on line {rule_match.start.line}"
        )
        return True, []

    # Strip quotes to allow for use of nosem as an HTML attribute inside tags.
    # HTML comments inside tags are not allowed by the spec.
    pattern_ids = {
        pattern_id.strip().strip("\"'") for pattern_id in ids if pattern_id.strip()
    }

    # Filter out ids that are not alphanum+dashes+underscores+periods.
    # This removes trailing symbols from comments, such as HTML comments `-->`
    # or C-like multiline comments `*/`.
    pattern_ids = set(filter(lambda x: not sub(r"[\w\-\.]+", "", x), pattern_ids))

    errors = []
    result = False
    for pattern_id in pattern_ids:
        if rule_match.rule_id == pattern_id:
            logger.verbose(
                f"found 'nosem' comment with id '{pattern_id}', skipping rule '{rule_match.rule_id}' on line {rule_match.start.line}"
            )
            result = result or True
        else:
            message = f"found 'nosem' comment with id '{pattern_id}', but no corresponding rule trying '{rule_match.rule_id}'"
            if strict:
                errors.append(SemgrepError(message, level=Level.WARN))
            else:
                logger.verbose(message)

    return result, errors
