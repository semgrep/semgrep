import hashlib
import json
from typing import Any
from typing import Dict
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v0 as out
from semgrep.constants import RuleSeverity
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch


def _to_gitlab_severity(semgrep_severity: RuleSeverity) -> str:
    # Todo: Semgrep states currently don't map super well to Gitlab schema.
    conversion_table: Dict[RuleSeverity, str] = {
        RuleSeverity.INFO: "Info",
        RuleSeverity.WARNING: "Medium",
        RuleSeverity.ERROR: "High",
    }
    return conversion_table.get(semgrep_severity, "Unknown")


def _construct_semgrep_rule_url(rule_id: str) -> str:
    # this is a hack to fix name -> registry disagreement
    components = rule_id.split(".")
    result = []
    for chunk in components:
        if chunk not in result:
            result.append(chunk)
    rule_name = ".".join(result)
    return f"https://semgrep.dev/r/{rule_name}"


class GitlabSastFormatter(BaseFormatter):
    def _format_rule_match(self, rule_match: RuleMatch) -> Mapping[str, Any]:
        # create UUID from sha256 hash
        return {
            "id": str(rule_match.uuid),
            "category": "sast",
            # CVE is a required field from Gitlab schema.
            # It also is part of the determination for uniqueness
            # of a detected secret
            # /regardless/ of differentiated ID. See issue 262648.
            # https://gitlab.com/gitlab-org/gitlab/-/issues/262648
            # Gitlab themselves mock a CVE for findings that lack a CVE
            # Format: path:hash-of-file-path:check_id
            "cve": "{}:{}:{}".format(
                rule_match.path,
                hashlib.sha256(bytes(rule_match.path)).hexdigest(),
                rule_match.rule_id,
            ),
            "message": rule_match.message,
            "severity": _to_gitlab_severity(rule_match.severity),
            # Semgrep is designed to be a low-FP tool by design.
            # Does hard-coding confidence make sense here?
            "confidence": "High",
            "scanner": {
                "id": "semgrep",
                "name": "Semgrep",
                "vendor": {"name": "Semgrep"},
            },
            "location": {
                "file": str(rule_match.path),
                # Gitlab only uses line identifiers
                "start_line": rule_match.start.line,
                "end_line": rule_match.end.line,
            },
            "identifiers": [
                {
                    "type": "semgrep_type",
                    "name": f"Semgrep - {rule_match.rule_id}",
                    "value": rule_match.rule_id,
                    "url": _construct_semgrep_rule_url(rule_match.rule_id),
                }
            ],
        }

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
    ) -> str:
        """
        Format matches in GitLab SAST report compliant JSON.

        - Written based on:
            https://github.com/returntocorp/semgrep-action/blob/678eff1a4269ed04b76631771688c8be860ec4e9/src/semgrep_agent/findings.py#L137-L165
        - Docs:
            https://docs.gitlab.com/ee/user/application_security/sast/#reports-json-format
        - Schema:
            https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/sast-report-format.json
        """
        output_dict = {
            "$schema": "https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/sast-report-format.json",
            "version": "14.1.2",
            "vulnerabilities": [
                self._format_rule_match(rule_match)
                for rule_match in rule_matches
                if rule_match.severity
                not in [RuleSeverity.INVENTORY, RuleSeverity.EXPERIMENT]
            ],
        }

        return json.dumps(output_dict, sort_keys=True, indent=2)
