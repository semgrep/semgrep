from typing import List

from semgrep.constants import CLI_RULE_ID
from semgrep.formatter.base import BaseFormatter
from semgrep.rule_match import RuleMatch


class EmacsFormatter(BaseFormatter):
    @staticmethod
    def _get_parts(rule_match: RuleMatch) -> List[str]:
        check_id = (
            rule_match.id.split(".")[-1] if rule_match.id != CLI_RULE_ID else None
        )
        severity = (
            rule_match.severity.lower() + f"({check_id})"
            if check_id
            else rule_match.severity.lower()
        )
        return [
            str(rule_match.path),
            str(rule_match.start["line"]),
            str(rule_match.start["col"]),
            severity,
            rule_match.lines[0].rstrip(),
        ]

    def output(self) -> str:
        rule_matches = sorted(self.rule_matches, key=lambda r: (r.path, r.id))
        return "\n".join(":".join(self._get_parts(rm)) for rm in rule_matches)
