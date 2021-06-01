import json
from typing import Any
from typing import Dict
from typing import List

from semgrep import __VERSION__
from semgrep.error import Level
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class SarifFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_sarif(rule_match: RuleMatch) -> Dict[str, Any]:
        return {
            "ruleId": rule_match.id,
            "message": {"text": rule_match.message},
            "locations": [
                {
                    "physicalLocation": {
                        "artifactLocation": {
                            "uri": str(rule_match.path),
                            "uriBaseId": "%SRCROOT%",
                        },
                        "region": {
                            "startLine": rule_match.start["line"],
                            "startColumn": rule_match.start["col"],
                            "endLine": rule_match.end["line"],
                            "endColumn": rule_match.end["col"],
                        },
                    }
                }
            ],
        }

    @staticmethod
    def _rule_to_sarif(rule: Rule) -> Dict[str, Any]:
        severity = SarifFormatter._rule_to_sarif_severity(rule)
        tags = SarifFormatter._rule_to_sarif_tags(rule)
        return {
            "id": rule.id,
            "name": rule.id,
            "shortDescription": {"text": rule.message},
            "fullDescription": {"text": rule.message},
            "defaultConfiguration": {"level": severity},
            "properties": {"precision": "very-high", "tags": tags},
        }

    @staticmethod
    def _rule_to_sarif_severity(rule: Rule) -> str:
        """
        SARIF v2.1.0-compliant severity string.

        See https://github.com/oasis-tcs/sarif-spec/blob/a6473580/Schemata/sarif-schema-2.1.0.json#L1566
        """
        mapping = {"INFO": "note", "ERROR": "error", "WARNING": "warning"}
        return mapping[rule.severity]

    @staticmethod
    def _rule_to_sarif_tags(rule: Rule) -> List[str]:
        """
        Tags to display on SARIF-compliant UIs, such as GitHub security scans.
        """
        result = []
        if "cwe" in rule.metadata:
            result.append(rule.metadata["cwe"])
        if "owasp" in rule.metadata:
            owasp = rule.metadata["owasp"]
            result.append(f"OWASP-{owasp}")
        return result

    @staticmethod
    def _semgrep_error_to_sarif_notification(error: SemgrepError) -> Dict[str, Any]:
        error_dict = error.to_dict()
        descriptor = error_dict["type"]

        error_to_sarif_level = {
            Level.ERROR.name.lower(): "error",
            Level.WARN.name.lower(): "warning",
        }
        level = error_to_sarif_level[error_dict["level"]]

        message = error_dict.get("message")
        if message is None:
            message = error_dict.get("long_msg")
        if message is None:
            message = error_dict.get("short_msg", "")

        return {
            "descriptor": {"id": descriptor},
            "message": {"text": message},
            "level": level,
        }

    def output(self) -> str:
        """
        Format matches in SARIF v2.1.0 formatted JSON.

        - Written based on:
            https://help.github.com/en/github/finding-security-vulnerabilities-and-errors-in-your-code/about-sarif-support-for-code-scanning
        - Which links to this schema:
            https://github.com/oasis-tcs/sarif-spec/blob/master/Schemata/sarif-schema-2.1.0.json
        - Full specification is at:
            https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html
        """

        output_dict = {
            "$schema": "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
            "version": "2.1.0",
            "runs": [
                {
                    "tool": {
                        "driver": {
                            "name": "semgrep",
                            "semanticVersion": __VERSION__,
                            "rules": [self._rule_to_sarif(rule) for rule in self.rules],
                        }
                    },
                    "results": [
                        self._rule_match_to_sarif(rule_match)
                        for rule_match in self.rule_matches
                    ],
                    "invocations": [
                        {
                            "executionSuccessful": True,
                            "toolExecutionNotifications": [
                                self._semgrep_error_to_sarif_notification(error)
                                for error in self.semgrep_structured_errors
                            ],
                        }
                    ],
                },
            ],
        }
        # Sort keys for predictable output. This helps with snapshot tests, etc.
        return json.dumps(output_dict, sort_keys=True)
