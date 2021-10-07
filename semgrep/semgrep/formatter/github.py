from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import Sequence

from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class GitHubFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_github(rule_match: RuleMatch) -> str:
        # Normalize the severity to match what github can handle. Default to notice
        severity_text = "notice"
        if rule_match.severity == RuleSeverity.INFO:
            pass
        elif rule_match.severity == RuleSeverity.WARNING:
            severity_text = "warning"
        elif rule_match.severity == RuleSeverity.ERROR:
            severity_text = "error"

        return "::{level} file={file},line={start},endLine={end},title={title}::{message}".format(
            level=severity_text,
            title=f"semgrep rule:{str(rule_match.id)}",
            file=str(rule_match.path),
            start=str(rule_match.start.line),
            end=str(rule_match.end.line),
            message=str(rule_match.extra["message"]),
        )

    def output(
        self,
        rules: FrozenSet[Rule],
        rule_matches: Sequence[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        extra: Mapping[str, Any],
    ) -> str:
        rule_matches = sorted(rule_matches, key=lambda r: (r.path, r.id))
        return "\n".join(self._rule_match_to_github(rm) for rm in rule_matches)
