from semgrep.constants import RuleSeverity
from semgrep.lsp.types import DiagnosticSeverity
from semgrep.types import JsonObject

SeverityMapping = {
    RuleSeverity.ERROR: DiagnosticSeverity.Error,
    RuleSeverity.WARNING: DiagnosticSeverity.Warning,
    RuleSeverity.INFO: DiagnosticSeverity.Information,
    RuleSeverity.INVENTORY: DiagnosticSeverity.Hint,
}


def findings_severity_to_diagnostic(severity: RuleSeverity) -> DiagnosticSeverity:
    return SeverityMapping.get(severity, DiagnosticSeverity.Information)


# TODO: need to use RuleMatch here instead of raw json/dict access
def findings_to_diagnostic(finding: JsonObject, content: str) -> JsonObject:
    from semgrep.state import get_state

    env = get_state().env
    start_line = finding["start"]["line"] - 1
    start_col = finding["start"]["col"] - 1
    end_line = finding["end"]["line"] - 1
    end_col = finding["end"]["col"] - 1
    lines = content.split("\n")
    if start_line == end_line:
        match_source = lines[start_line][start_col:end_col]
    else:
        middle_lines = "\n".join(lines[start_line + 1 : end_line])
        match_source = (
            lines[start_line][start_col:] + middle_lines + lines[end_line][:end_col]
        )

    rule_url = f"{env.semgrep_url}/r/{finding['check_id']}"

    diagnostic = {
        "range": {
            "start": {"line": start_line, "character": start_col},
            "end": {
                "line": end_line,
                "character": end_col,
            },
        },
        "message": finding["extra"]["message"],
        "severity": findings_severity_to_diagnostic(
            RuleSeverity(finding["extra"]["severity"])
        ).value,
        "source": "Semgrep",
        "code": finding["check_id"],
        "codeDescription": {"href": rule_url},
        "data": {"matchSource": match_source},
    }

    if "fix" in finding["extra"]:
        diagnostic["data"]["fix"] = finding["extra"]["fix"]
    if "fix_regex" in finding["extra"]:
        diagnostic["data"]["fix_regex"] = finding["extra"]["fix_regex"]

    return diagnostic
