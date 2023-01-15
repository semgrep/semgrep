from logging import getLogger
from pathlib import Path
from typing import Sequence
from typing import Set
from typing import Tuple

from semgrep.lsp.config import LSPConfig
from semgrep.rule_match import RuleMatchMap

log = getLogger(__name__)


def run_rules(
    targets: Sequence[str], config: LSPConfig
) -> Tuple[RuleMatchMap, Set[Path]]:
    (
        filtered_matches_by_rule,
        _,
        all_targets,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
    ) = config.scanner(target=targets)
    # ignore this type since we're doing weird things with partial :O
    return (filtered_matches_by_rule, all_targets)


def run_rules_ci(
    targets: Sequence[str], config: LSPConfig
) -> Tuple[RuleMatchMap, Set[Path]]:
    (
        filtered_matches_by_rule,
        _,
        all_targets,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
    ) = config.scanner_ci(target=targets)
    # ignore this type since we're doing weird things with partial :O
    return (filtered_matches_by_rule, all_targets)
