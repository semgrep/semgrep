import json
import urllib
from typing import Any
from typing import List
from typing import MutableMapping

from semgrep.constants import RuleSeverity
from semgrep.lsp.types import Diagnostic
from semgrep.lsp.types import DiagnosticSeverity
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchMap
from semgrep.semgrep_interfaces.semgrep_output_v1 import MetavarValue
from semgrep.target_manager import TargetManager
from semgrep.types import JsonObject
from semgrep.util import flatten


SeverityMapping = {
    RuleSeverity.ERROR: DiagnosticSeverity.Error,
    RuleSeverity.WARNING: DiagnosticSeverity.Warning,
    RuleSeverity.INFO: DiagnosticSeverity.Information,
    RuleSeverity.INVENTORY: DiagnosticSeverity.Hint,
    RuleSeverity.EXPERIMENT: DiagnosticSeverity.Hint,
}


def findings_severity_to_diagnostic(severity: RuleSeverity) -> DiagnosticSeverity:
    """Convert a rule severity to a LSP diagnostic severity"""
    return SeverityMapping.get(severity, DiagnosticSeverity.Information)


# According to https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#inlayHint
def metavar_to_inlay(metavar: str, info: MetavarValue) -> JsonObject:
    """Convert a Semgrep metavar to an inlay hint"""
    return {
        "position": {
            "line": info.start.line - 1,
            "character": info.start.col - 1,
        },
        "label": f"{metavar}:",
        "tooltip": info.abstract_content,
        "paddingRight": True,
    }


# According to https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeAction
def diagnostic_to_code_action(
    uri: str, diagnostic: Diagnostic, newText: str
) -> JsonObject:
    """Convert a LSP diagnostic to a code action"""
    check_id = diagnostic["code"]
    code_action = {
        "title": f"Apply fix suggested by Semgrep rule {check_id}",
        "kind": "quickfix",
        "edit": {
            "changes": {uri: [{"range": diagnostic["range"], "newText": newText}]}
        },
    }
    return code_action


# Right now this is just the location + abstract content of metavars
def rule_match_get_related(rule_match: RuleMatch) -> List[JsonObject]:
    """Get related info (metavars + locations) for a rule match"""

    # According to https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnosticRelatedInformation
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


# According to https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic
def rule_match_to_diagnostic(rule_match: RuleMatch) -> Diagnostic:
    """Convert a rule match to a LSP diagnostic"""
    # we should try and link to file vs ci here depending on rule origin
    rule_url = rule_match.metadata.get("shortlink")
    if not rule_url:
        # Yes this is vscode specific but if you use another editor
        # You probably won't be clicking links :)
        args = {"query": rule_match.rule_id, "includes": "*.{yml,yaml}"}
        rule_url = f"command:search.action.openNewEditorToSide?{urllib.parse.quote(json.dumps([args]))}"
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
            # Use match_formula_string here since it'll always be more accurate
            # for fix_regex
            "matchSource": rule_match.match_formula_string,
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
        diagnostic["data"]["fix_regex"] = rule_match.fix_regex.to_json()
        fix_message = rule_match.fix

    if fix_message is not None:
        diagnostic["message"] += f"\nFix: {fix_message}"
    # This looks better
    diagnostic["message"] += "\n"
    return diagnostic


def rule_match_map_to_diagnostics(rule_map: RuleMatchMap) -> List[Diagnostic]:
    return flatten(
        [
            list(map(rule_match_to_diagnostic, rule_match_list))
            for rule_match_list in rule_map.values()
        ]
    )


def rule_to_files(rule: Rule, target_manager: TargetManager) -> List[str]:
    """Get assocaitated files for a rule match"""
    target_files = []
    for lang in rule.languages:
        files = target_manager.get_files_for_rule(lang, [], [], rule_id=rule.id)
        for f in files:
            target_files.append(str(f))
    return target_files


# This is a custom data type :)
def rule_to_metadata(rule: Rule) -> JsonObject:
    """Get metadata for a rule to be used in the LSP"""
    return {
        "id": rule.id,
        "languages": rule.languages,
        "message": rule.message,
        "severity": str(rule.severity),
        "metadata": rule.metadata,
        "include": rule.includes,
        "exclude": rule.excludes,
        "is_blocking": rule.is_blocking,
    }
