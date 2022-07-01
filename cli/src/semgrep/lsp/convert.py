from typing import Any
from typing import List
from typing import MutableMapping

from semgrep.constants import RuleSeverity
from semgrep.lsp.types import DiagnosticSeverity
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_interfaces.semgrep_output_v0 import MetavarValue
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
from semgrep.types import JsonObject
from semgrep.util import flatten

SeverityMapping = {
    RuleSeverity.ERROR: DiagnosticSeverity.Error,
    RuleSeverity.WARNING: DiagnosticSeverity.Warning,
    RuleSeverity.INFO: DiagnosticSeverity.Information,
    RuleSeverity.INVENTORY: DiagnosticSeverity.Hint,
}


def findings_severity_to_diagnostic(severity: RuleSeverity) -> DiagnosticSeverity:
    return SeverityMapping.get(severity, DiagnosticSeverity.Information)


def metavar_to_inlay(metavar: str, info: MetavarValue) -> JsonObject:
    return {
        "position": {
            "line": info.start.line - 1,
            "character": info.start.col - 1,
        },
        "label": f"{metavar}:",
        "tooltip": info.abstract_content,
        "paddingRight": True,
    }


# Get related info for a rule match
# Right now this is just the location + abstract content of metavars
def rule_match_get_related(rule_match: RuleMatch) -> List[JsonObject]:
    def get_metavar_related(m: str, d: MetavarValue) -> JsonObject:
        uri = f"file://{rule_match.path}"
        related = {
            "location": {
                "uri": uri,
                "range": {
                    # We start at 0, but the LSP starts at 1
                    "start": {"line": d.start.line - 1, "character": d.start.col - 1},
                    "end": {
                        "line": d.end.line - 1,
                        "character": d.end.col - 1,
                    },
                },
            },
            "message": f"{m}: {d.abstract_content}",
        }
        return related

    if rule_match.extra.get("metavars") is not None:
        return list(
            get_metavar_related(m, MetavarValue.from_json(d))
            for m, d in rule_match.extra["metavars"].items()
        )
    return []


# Convert a rule match to a LSP diagnostic
# According to https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
def rule_match_to_diagnostic(rule_match: RuleMatch) -> JsonObject:
    env = get_state().env
    rule_url = f"{env.semgrep_url}/r/{rule_match.rule_id}"
    diagnostic: MutableMapping[str, Any] = {
        "range": {
            # We start at 0, but the LSP starts at 1
            "start": {
                "line": rule_match.start.line - 1,
                "character": rule_match.start.col - 1,
            },
            "end": {
                "line": rule_match.end.line - 1,
                "character": rule_match.end.col - 1,
            },
        },
        "message": rule_match.message,
        "severity": findings_severity_to_diagnostic(rule_match.severity).value,
        "source": "Semgrep",
        "code": rule_match.rule_id,
        "codeDescription": {"href": rule_url},
        "data": {
            "matchSource": rule_match.syntactic_context,
            "uri": f"file://{rule_match.path}",
        },
        "relatedInformation": rule_match_get_related(rule_match),
    }

    fix_message = None
    if rule_match.extra.get("metavars") is not None:
        diagnostic["data"]["metavars"] = rule_match.extra["metavars"]
    if rule_match.fix:
        diagnostic["data"]["fix"] = rule_match.fix
        fix_message = rule_match.fix
    if rule_match.fix_regex:
        diagnostic["data"]["fix_regex"] = rule_match.fix_regex
        fix_message = rule_match.fix

    if fix_message is not None:
        diagnostic["message"] += f"\nFix: {fix_message}"
    # This looks better
    diagnostic["message"] += "\n"
    return diagnostic


def rule_match_map_to_diagnostics(rule_map: RuleMatchMap) -> List[JsonObject]:
    return flatten(
        [
            list(map(rule_match_to_diagnostic, rule_match_list))
            for rule_match_list in rule_map.values()
        ]
    )


# get assocaitated files for a rule match
def rule_to_files(rule: Rule, target_manager: TargetManager) -> List[str]:
    target_files = []
    for lang in rule.languages:
        files = target_manager.get_files_for_rule(lang, [], [], rule_id=rule.id)
        for f in files:
            target_files.append(str(f))
    return target_files
