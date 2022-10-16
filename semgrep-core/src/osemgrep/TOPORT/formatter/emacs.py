from semgrep.constants import CLI_RULE_ID

class EmacsFormatter(BaseFormatter):
    @staticmethod
    def _get_parts(rule_match: RuleMatch) -> Sequence[str]:
        check_id = (
            rule_match.rule_id.split(".")[-1]
            if rule_match.rule_id != CLI_RULE_ID
            else None
        )
        match_severity = rule_match.severity.value.lower()
        severity = match_severity + f"({check_id})" if check_id else match_severity
        return [
            str(rule_match.path),
            str(rule_match.start.line),
            str(rule_match.start.col),
            severity,
            rule_match.lines[0].rstrip() if rule_match.lines else "",
            rule_match.message,
        ]

    def format(
        self,
        rules: Iterable[Rule],
        rule_matches: Iterable[RuleMatch],
        semgrep_structured_errors: Sequence[SemgrepError],
        cli_output_extra: out.CliOutputExtra,
        extra: Mapping[str, Any],
    ) -> str:
        sorted_matches = sorted(rule_matches, key=lambda r: (r.path, r.rule_id))
        return "\n".join(":".join(self._get_parts(rm)) for rm in sorted_matches)
