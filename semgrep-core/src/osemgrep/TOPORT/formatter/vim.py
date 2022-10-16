
class VimFormatter(BaseFormatter):
    @staticmethod
    def _get_parts(rule_match: RuleMatch) -> Sequence[str]:
        severity = {
            RuleSeverity.INFO: "I",
            RuleSeverity.WARNING: "W",
            RuleSeverity.ERROR: "E",
        }
        return [
            str(rule_match.path),
            str(rule_match.start.line),
            str(rule_match.start.col),
            severity[rule_match.severity],
            rule_match.rule_id,
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
        return "\n".join(":".join(self._get_parts(rm)) for rm in rule_matches)
