import hashlib
import json
from datetime import datetime
from typing import Any
from typing import Dict
from typing import Iterable
from typing import Mapping
from typing import Sequence

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.error import SemgrepError
from semgrep.formatter.base import BaseFormatter
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.state import get_state


def _to_gitlab_severity(semgrep_severity: out.MatchSeverity) -> str:
    conversion_table: Dict[out.MatchSeverity, str] = {
        out.MatchSeverity(out.Info()): "Info",
        out.MatchSeverity(out.Low()): "Low",
        out.MatchSeverity(out.Warning()): "Medium",
        out.MatchSeverity(out.Medium()): "Medium",
        out.MatchSeverity(out.Error()): "High",
        out.MatchSeverity(out.High()): "High",
        out.MatchSeverity(out.Critical()): "Critical",
    }
    return conversion_table.get(semgrep_severity, "Unknown")


class GitlabSastFormatter(BaseFormatter):
    def _format_rule_match(self, rule_match: RuleMatch) -> Mapping[str, Any]:
        result: Dict[str, Any] = {
            "id": str(rule_match.uuid),  # create UUID from sha256 hash
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
            # added to the GitLab SAST schema in 16.x, c.f.
            # https://gitlab.com/gitlab-org/gitlab/-/issues/339812
            # The above "message" field should be unused in 16.x and later!
            "description": rule_match.message,
            "severity": _to_gitlab_severity(rule_match.severity),
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
                    "url": rule_match.metadata.get("source"),
                }
            ],
            "flags": [],
            "details": {},
        }

        confidence = rule_match.metadata.get("confidence")
        if confidence:
            result["details"]["confidence"] = {
                "type": "text",
                "name": "confidence",
                "value": confidence,
            }
            if confidence == "LOW":
                result["flags"].append(
                    {
                        "type": "flagged-as-likely-false-positive",
                        "origin": "Semgrep",
                        "description": "This finding is from a low confidence rule.",
                    }
                )

        if rule_match.exposure_type:
            result["details"]["exposure"] = {
                "type": "text",
                "name": "exposure",
                "value": rule_match.exposure_type,
            }
            if rule_match.exposure_type == "unreachable":
                result["flags"].append(
                    {
                        "type": "flagged-as-likely-false-positive",
                        "origin": "Semgrep Supply Chain",
                        "description": (
                            "Semgrep found no way to reach this vulnerability "
                            "while scanning your code."
                        ),
                    }
                )

        return result

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
        is_ci_invocation: bool,
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
        metrics = get_state().metrics
        start_time = datetime.fromisoformat(metrics.payload.started_at.value)
        start_time = start_time.replace(tzinfo=None)
        output_dict = {
            "$schema": "https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/sast-report-format.json",
            "version": "15.0.4",
            "scan": {
                "start_time": start_time.isoformat(
                    timespec="seconds",
                ),
                "end_time": datetime.now().isoformat(timespec="seconds"),
                "analyzer": {
                    "id": "semgrep",
                    "name": "Semgrep",
                    "url": "https://semgrep.dev",
                    "version": metrics.payload.environment.version,
                    "vendor": {"name": "Semgrep"},
                },
                "scanner": {
                    "id": "semgrep",
                    "name": "Semgrep",
                    "url": "https://semgrep.dev",
                    "version": metrics.payload.environment.version,
                    "vendor": {"name": "Semgrep"},
                },
                "version": metrics.payload.environment.version,
                "status": "success",
                "type": "sast",
            },
            "vulnerabilities": [
                self._format_rule_match(rule_match)
                for rule_match in rule_matches
                if rule_match.severity.value not in [out.Inventory(), out.Experiment()]
            ],
        }

        return json.dumps(output_dict, sort_keys=True, indent=2)
