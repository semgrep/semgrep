import semgrep.error as error
from semgrep.rule_match import RuleMatch
from semgrep.rule_match_map import RuleMatchMap
from typing import Any, Sequence, Tuple

def process_ignores(rule_matches_by_rule: RuleMatchMap,
                    keep_ignored: bool,
                    strict: bool) -> Tuple[RuleMatchMap,
                                           Sequence[error.SemgrepError], int]:
    """
    Handles ignoring Semgrep findings via inline code comments
    Currently supports ignoring a finding on a single line by adding a
    `# nosemgrep:ruleid` comment (or `// nosemgrep:ruleid`).
    To use, create a RuleMatchMap, then pass it to process_ignores().
    """
    ...
