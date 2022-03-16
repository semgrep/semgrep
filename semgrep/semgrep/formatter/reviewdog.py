import json
import re
from typing import Any, Iterable, Mapping, Sequence

from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch

SEVERITIES = [
    RuleSeverity.INVENTORY.value, 
    RuleSeverity.INFO.value, 
    RuleSeverity.WARNING.value, 
    RuleSeverity.ERROR.value
]


class ReviewdogFormatter(BaseFormatter):
    """
    Output formatter for the Reviewdog Diagnostic Format (rdformat).
    Format spec: https://github.com/reviewdog/reviewdog/tree/master/proto/rdf
    """
    @staticmethod
    def _rule_match_to_json(rule_match: RuleMatch) -> Mapping[str, Any]:
        location = {}
        location["path"] = str(rule_match.path)
        range = {}
        range["start"] = {}
        range["start"]["column"] = rule_match.start.col
        range["start"]["line"] = rule_match.start.line
        range["end"] = {}
        range["end"]["column"] = rule_match.end.col
        range["end"]["line"] = rule_match.end.line
        location["range"] = range
        code = {}
        code["value"] = rule_match.id
        code["url"] = ""
        suggestions = None

        # basic fix
        if rule_match.fix:
            suggestions = {}
            suggestions["range"] = range
            suggestions["text"] = rule_match.fix
        
        # regex-based fix
        if rule_match.fix_regex:
            suggestions = {}
            fix = re.compile(rule_match.fix_regex["regex"])
            replacement = fix.sub(
                rule_match.fix_regex["replacement"], 
                "".join(rule_match.lines).rstrip()
            )
            suggestions["text"] = replacement
            suggestions["range"] = range
        
        diagnostic = {
            "message": rule_match.message,
            "location": location,
            "code": code,
            "severity": rule_match.severity.value,
        }
        if suggestions is not None:
            diagnostic["suggestions"] = [suggestions]
        return diagnostic

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        extra: Mapping[str, Any],
    ) -> str:
        output_dict = {
            "source": {"name": "semgrep", "url": "https://semgrep.dev/"},
            "diagnostics": [
                self._rule_match_to_json(rule_match) for rule_match in rule_matches
            ],
            "errors": [error.to_dict() for error in semgrep_structured_errors],
            **extra,
        }
        # set overall severity to the highest value found in matches
        output_dict["severity"] = SEVERITIES[max(SEVERITIES.index(d["severity"]) for d in output_dict["diagnostics"])]
        # Sort keys for predictable output. This helps with snapshot tests, etc.
        return json.dumps(output_dict, sort_keys=True)
