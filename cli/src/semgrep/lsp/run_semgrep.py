from logging import getLogger
from typing import Sequence

from semgrep.lsp.config import LSPConfig
from semgrep.rule_match import RuleMatchMap

log = getLogger(__name__)


def run_rules(targets: Sequence[str], config: LSPConfig) -> RuleMatchMap:
    (
        filtered_matches_by_rule,
        _,
        _,
        _,
        filtered_rules,
        profiler,
        profiling_data,
        shown_severities,
    ) = config.scanner(target=targets)
    # ignore this type since we're doing weird things with partial :O
    return filtered_matches_by_rule  # type: ignore


def run_rules_ci(targets: Sequence[str], config: LSPConfig) -> RuleMatchMap:
    (
        filtered_matches_by_rule,
        _,
        _,
        _,
        filtered_rules,
        profiler,
        profiling_data,
        shown_severities,
    ) = config.scanner_ci(target=targets)
    # ignore this type since we're doing weird things with partial :O
    return filtered_matches_by_rule  # type: ignore
