from typing import Any
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v0 as out
from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class GithubActionsFormatter(BaseFormatter):
    @staticmethod
    def _get_parts(rule_match: RuleMatch) -> str:
        severity = {
            RuleSeverity.INFO: "notice",
            RuleSeverity.WARNING: "warning",
            RuleSeverity.ERROR: "error",
        }[rule_match.severity]
        return "::{severity} file={path},line={line},col={col},endLine={endLine},endCol={endCol}::{message}".format(
          severity=severity,
          path=rule_match.path,
          line=rule_match.start.line,
          col=rule_match.start.col,
          endLine=rule_match.end.line,
          endCol=rule_match.end.col,
          message=rule_match.message,
        )

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
    ) -> str:
        return "\n".join(self._get_parts(rm) for rm in rule_matches)
