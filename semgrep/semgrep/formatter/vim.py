from typing import List

from semgrep.formatter.base import BaseFormatter
from semgrep.rule_match import RuleMatch


class VimFormatter(BaseFormatter):
    @staticmethod
    def _get_parts(rule_match: RuleMatch) -> List[str]:
        severity = {
            "INFO": "I",
            "WARNING": "W",
            "ERROR": "E",
        }
        return [
            str(rule_match.path),
            str(rule_match.start["line"]),
            str(rule_match.start["col"]),
            severity[rule_match.severity],
            rule_match.id,
            rule_match.message,
        ]

    def output(self) -> str:
        return "\n".join(":".join(self._get_parts(rm)) for rm in self.rule_matches)
