import json
from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import Sequence

from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class JsonFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_json(rule_match: RuleMatch) -> Mapping[str, Any]:
        extra = rule_match.extra
        extra["message"] = rule_match.message
        extra["metadata"] = rule_match.metadata
        extra["severity"] = rule_match.severity.value

        # 'lines' already contains '\n' at the end of each line
        extra["lines"] = "".join(rule_match.lines).rstrip()

        if rule_match.fix:
            extra["fix"] = rule_match.fix
        if rule_match.fix_regex:
            extra["fix_regex"] = rule_match.fix_regex
        if rule_match.is_ignored is not None:
            extra["is_ignored"] = rule_match.is_ignored

        return {
            "check_id": rule_match.id,
            "path": str(rule_match.path),
            "start": rule_match.start.to_dict(),
            "end": rule_match.end.to_dict(),
            "extra": extra,
        }

    def output(
        self,
        rules: FrozenSet[Rule],
        rule_matches: Sequence[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        extra: Mapping[str, Any],
    ) -> str:
        output_dict = {
            "results": [
                self._rule_match_to_json(rule_match) for rule_match in rule_matches
            ],
            "errors": [error.to_dict() for error in semgrep_structured_errors],
            **extra,
        }
        # Sort keys for predictable output. This helps with snapshot tests, etc.
        return json.dumps(output_dict, sort_keys=True)
