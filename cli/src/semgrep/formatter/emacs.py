from typing import Any
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.constants import CLI_RULE_ID
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class EmacsFormatter(BaseFormatter):
    @staticmethod
    def _get_parts(rule_match: RuleMatch) -> Sequence[str]:
        check_id = (
            rule_match.rule_id.split(".")[-1]
            if rule_match.rule_id != CLI_RULE_ID
            else None
        )
        match_severity = rule_match.severity.value.to_json().lower()
        severity = match_severity + f"({check_id})" if check_id else match_severity
        return [
            str(rule_match.path),
            str(rule_match.start.line),
            str(rule_match.start.col),
            severity,
            rule_match.lines[0].rstrip() if rule_match.lines else "",
            rule_match.message,
        ]

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        is_ci_invocation: bool,
    ) -> str:
        sorted_matches = sorted(rule_matches, key=lambda r: (r.path, r.rule_id))
        return "\n".join(":".join(self._get_parts(rm)) for rm in sorted_matches)
