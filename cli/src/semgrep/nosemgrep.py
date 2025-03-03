"""
Handles ignoring Semgrep findings via inline code comments

Currently supports ignoring a finding on a single line by adding a
`# nosemgrep:ruleid` comment (or `// nosemgrep:ruleid`).

This code currently just consumes the `is_ignored` field, which is set
on each match by the Core Engine.

Coupling: semgrep-action uses a regexp to strip nosemgrep comments so as
to normalize the code and not be sensitive to the addition or removal
of nosemgrep comments.
See https://github.com/returntocorp/semgrep-action/blob/develop/src/semgrep_agent/findings.py
and check that it's compatible with any change we're making here.
"""
from boltons.iterutils import partition

from semgrep.rule_match import RuleMatchMap
from semgrep.types import FilteredMatches
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def filter_ignored(
    rule_matches_by_rule: RuleMatchMap,
    *,
    keep_ignored: bool,
) -> FilteredMatches:
    """
    Converts a mapping of findings to a mapping of findings that
    will be shown to the caller.

    :param rule_matches_by_rule: The input findings (typically from a Semgrep call)
    :param keep_ignored: if true will keep nosem findings in returned object, otherwise removes them
    :return:
    - FilteredMatches: dicts from rule to list of findings. Findings have is_ignored
        set to true if there was matching nosem comment found for it.
        If keep_ignored set to true, will keep all findings that have is_ignored: True
        in the .kept attribute, otherwise moves them to .removed
    """
    result = FilteredMatches(rule_matches_by_rule)
    for rule, matches in rule_matches_by_rule.items():
        result.kept[rule], result.removed[rule] = partition(
            matches,
            lambda match: keep_ignored or not match.match.extra.is_ignored,
        )

    return result
