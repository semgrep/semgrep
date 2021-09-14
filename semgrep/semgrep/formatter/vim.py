from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import Sequence

from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class VimFormatter(BaseFormatter):
    @staticmethod
    def _get_parts(rule_match: RuleMatch) -> Sequence[str]:
        severity = {
            RuleSeverity.INFO: "I",
            RuleSeverity.WARNING: "W",
            RuleSeverity.ERROR: "E",
        }
        return [
            str(rule_match.path),
            str(rule_match.start.line),
            str(rule_match.start.col),
            severity[rule_match.severity],
            rule_match.id,
            rule_match.message,
        ]

    def output(
        self,
        rules: FrozenSet[Rule],
        rule_matches: Sequence[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        extra: Mapping[str, Any],
    ) -> str:
        return "\n".join(":".join(self._get_parts(rm)) for rm in rule_matches)
