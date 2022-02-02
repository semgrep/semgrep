from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from pathlib import Path
from semgrep.rule_match import RuleMatch
from semgrep.rule import Rule
import semgrep.error as error

def run_dependency_aware_rule(
    matches: List[RuleMatch],
    rule: Rule,
    targets: List[Path],
) -> Tuple[List[RuleMatch], List[error.SemgrepError]]:
    """
    Run a dependency aware rule. These rules filters the results based on the precense or absence
    of dependencies. Dependencies are determined by searching for lockfiles under the first entry
    in the `targets` argument.
    """
    ...
