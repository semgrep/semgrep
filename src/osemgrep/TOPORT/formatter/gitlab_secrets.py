from typing import Any
from typing import Mapping

from semgrep.formatter.gitlab_sast import GitlabSastFormatter
from semgrep.rule_match import RuleMatch


class GitlabSecretsFormatter(GitlabSastFormatter):
    def _format_rule_match(self, rule_match: RuleMatch) -> Mapping[str, Any]:
        return {
            **super()._format_rule_match(rule_match),
            "category": "secret_detection",
            "raw_source_code_extract": rule_match.lines,
            "commit": {
                "date": "1970-01-01T00:00:00Z",
                # Even the native Gitleaks based Gitlab secret detection
                # only provides a dummy value for now on relevant hash.
                "sha": "0000000",
            },
        }
