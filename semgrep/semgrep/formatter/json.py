import json
from typing import Any
from typing import Dict

from semgrep.formatter.base import BaseFormatter
from semgrep.rule_match import RuleMatch


class JsonFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_json(rule_match: RuleMatch) -> Dict[str, Any]:
        json_obj: Dict[str, Any] = {}

        json_obj["check_id"] = rule_match.id
        json_obj["extra"] = rule_match.extra
        json_obj["extra"]["message"] = rule_match.message
        json_obj["extra"]["metadata"] = rule_match.metadata
        json_obj["extra"]["severity"] = rule_match.severity.value
        json_obj["path"] = str(rule_match.path)

        json_obj["start"] = {"line": rule_match.start.line, "col": rule_match.start.col}
        json_obj["end"] = {"line": rule_match.end.line, "col": rule_match.end.col}

        # 'lines' already contains '\n' at the end of each line
        json_obj["extra"]["lines"] = "".join(rule_match.lines).rstrip()

        if rule_match.fix:
            json_obj["extra"]["fix"] = rule_match.fix
        if rule_match.fix_regex:
            json_obj["extra"]["fix_regex"] = rule_match.fix_regex
        if rule_match.is_ignored is not None:
            json_obj["extra"]["is_ignored"] = rule_match.is_ignored

        return json_obj

    def output(self) -> str:
        output_dict = {
            "results": [
                self._rule_match_to_json(rule_match) for rule_match in self.rule_matches
            ],
            "errors": [error.to_dict() for error in self.semgrep_structured_errors],
            **self.extra,
        }
        # Sort keys for predictable output. This helps with snapshot tests, etc.
        return json.dumps(output_dict, sort_keys=True)
