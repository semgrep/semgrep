from logging import getLogger
from typing import Sequence

from semgrep.lsp.config import LSPConfig
from semgrep.rule_match import RuleMatchMap

log = getLogger(__name__)


def run_rules(targets: Sequence[str], config: LSPConfig) -> RuleMatchMap:
    # ignore this type since we're doing weird things with partial :O
    return config.scanner(target=targets)[0]  # type: ignore
