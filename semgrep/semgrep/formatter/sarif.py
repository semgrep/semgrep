import json
from typing import Any
from typing import FrozenSet
from typing import Mapping
from typing import Sequence

from semgrep import __VERSION__
from semgrep.constants import RuleSeverity
from semgrep.error import Level
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


class SarifFormatter(BaseFormatter):
    @staticmethod
    def _rule_match_to_sarif(rule_match: RuleMatch) -> Mapping[str, Any]:
        rule_match_sarif = {
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
                            "startLine": rule_match.start.line,
                            "startColumn": rule_match.start.col,
                            "endLine": rule_match.end.line,
                            "endColumn": rule_match.end.col,
                        },
                    }
                }
            ],
        }
        if rule_match._is_ignored:
            rule_match_sarif["suppressions"] = [{"kind": "inSource"}]
        return rule_match_sarif

    @staticmethod
    def _rule_to_sarif(rule: Rule) -> Mapping[str, Any]:
        severity = SarifFormatter._rule_to_sarif_severity(rule)
        tags = SarifFormatter._rule_to_sarif_tags(rule)
        rule_json = {
            "id": rule.id,
            "name": rule.id,
            "shortDescription": {"text": rule.message},
            "fullDescription": {"text": rule.message},
            "defaultConfiguration": {"level": severity},
            "properties": {"precision": "very-high", "tags": tags},
        }

        rule_url = rule.metadata.get("source")
        if rule_url is not None:
            rule_json["helpUri"] = rule_url

        rule_short_description = rule.metadata.get("shortDescription")
        if rule_short_description:
            rule_json["shortDescription"] = {"text": rule_short_description}

        rule_help_text = rule.metadata.get("help")
        if rule_help_text:
            rule_json["help"] = {"text": rule_help_text}

        return rule_json

    @staticmethod
    def _rule_to_sarif_severity(rule: Rule) -> str:
        """
        SARIF v2.1.0-compliant severity string.

        See https://github.com/oasis-tcs/sarif-spec/blob/a6473580/Schemata/sarif-schema-2.1.0.json#L1566
        """
        mapping = {
            RuleSeverity.INFO: "note",
            RuleSeverity.WARNING: "warning",
            RuleSeverity.ERROR: "error",
        }
        return mapping[rule.severity]

    @staticmethod
    def _rule_to_sarif_tags(rule: Rule) -> Sequence[str]:
        """
        Tags to display on SARIF-compliant UIs, such as GitHub security scans.
        """
        result = []
        if "cwe" in rule.metadata:
            result.append(rule.metadata["cwe"])
        if "owasp" in rule.metadata:
            owasp = rule.metadata["owasp"]
            result.append(f"OWASP-{owasp}")

        for tags in rule.metadata.get("tags", []):
            result.append(tags)

        return result

    @staticmethod
    def _semgrep_error_to_sarif_notification(error: SemgrepError) -> Mapping[str, Any]:
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

    def keep_ignores(self) -> bool:
        # SARIF output includes ignored findings, but labels them as suppressed.
        # https://docs.oasis-open.org/sarif/sarif/v2.1.0/csprd01/sarif-v2.1.0-csprd01.html#_Toc10541099
        return True

    def output(
        self,
        rules: FrozenSet[Rule],
        rule_matches: Sequence[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        extra: Mapping[str, Any],
    ) -> str:
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
                            "rules": [self._rule_to_sarif(rule) for rule in rules],
                        }
                    },
                    "results": [
                        self._rule_match_to_sarif(rule_match)
                        for rule_match in rule_matches
                    ],
                    "invocations": [
                        {
                            "executionSuccessful": True,
                            "toolExecutionNotifications": [
                                self._semgrep_error_to_sarif_notification(error)
                                for error in semgrep_structured_errors
                            ],
                        }
                    ],
                },
            ],
        }
        # Sort keys for predictable output. This helps with snapshot tests, etc.
        return json.dumps(output_dict, sort_keys=True)
