from __future__ import annotations

from logging import getLogger
from pathlib import Path
from typing import Sequence

from semgrep.lsp.config import LSPConfig
from semgrep.rule_match import RuleMatchMap

log = getLogger(__name__)


def run_rules(
    targets: Sequence[str], config: LSPConfig
) -> tuple[RuleMatchMap, set[Path]]:
    (
        filtered_matches_by_rule,
        _,
        _,
        _,
        _,
        _,
        output_extra,
        _,
        _,
    ) = config.scanner(target=targets)
    # ignore this type since we're doing weird things with partial :O
    return (filtered_matches_by_rule, output_extra.all_targets)


def run_rules_ci(
    targets: Sequence[str], config: LSPConfig
) -> tuple[RuleMatchMap, set[Path]]:
    (
        filtered_matches_by_rule,
        _,
        _,
        _,
        _,
        _,
        output_extra,
        _,
        _,
    ) = config.scanner_ci(target=targets)
    # ignore this type since we're doing weird things with partial :O
    return (filtered_matches_by_rule, output_extra.all_targets)
