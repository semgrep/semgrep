from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import Sequence

from semgrep.constants import CLI_RULE_ID
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class GitHubFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_github(rule_match: RuleMatch) -> str:
        return '::{level} file={file},line={start},endLine={end},title={title}::{message}'.format(
            level   = str(rule_match.severity.value.lower()), # FIXME: this needs to be filtered?
            title   = str(rule_match.id),
            file    = str(rule_match.path),
            start   = str(rule_match.start.line),
            end     = str(rule_match.end.line),
            message = str(rule_match.extra["message"]), # FIXME: This could be prettier
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
