from semgrep.error import Level
from semgrep.types import JsonObject

def core_error_to_semgrep_error(err: core.CoreError) -> SemgrepCoreError:
    if isinstance(err.error_type.value, core.PatternParseError):
        yaml_path = err.error_type.value.value[::-1]
        error_span = _core_location_to_error_span(err.location)
        config_start = out.PositionBis(line=0, col=1)
        config_end = out.PositionBis(
            line=err.location.end.line - err.location.start.line,
            col=err.location.end.col - err.location.start.col + 1,
        )
        spans = [
            dataclasses.replace(
                error_span,
                config_start=config_start,
                config_end=config_end,
                config_path=yaml_path,
            )
        ]
    ...
    return SemgrepCoreError(code, level, spans, err)


def core_matches_to_rule_matches(
    rules: List[Rule], res: core.CoreMatchResults
) -> Dict[Rule, List[RuleMatch]]:

    def convert_to_rule_match(match: core.CoreMatch) -> RuleMatch:
        # this validation for fix_regex code was in autofix.py before
        # TODO: this validation should be done in rule.py when parsing the rule
        if rule.fix_regex:
            regex = rule.fix_regex.get("regex")
            replacement = rule.fix_regex.get("replacement")
            count = rule.fix_regex.get("count")
            if not regex or not replacement:
                raise SemgrepError(
                    "'regex' and 'replacement' values required when using 'fix-regex'"
                )
            if count:
                try:
                    count = int(count)
                except ValueError:
                    raise SemgrepError(
                        "optional 'count' value must be an integer when using 'fix-regex'"
                    )

            fix_regex = out.FixRegex(regex=regex, replacement=replacement, count=count)

        return RuleMatch(...)

    findings: Dict[Rule, RuleMatchSet] = {rule: RuleMatchSet(rule) for rule in rules}
    seen_cli_unique_keys: Set[Tuple] = set()
    for match in res.matches:
        rule = rule_table[match.rule_id.value]
        rule_match = convert_to_rule_match(match)
        if rule_match.cli_unique_key in seen_cli_unique_keys:
            continue
        seen_cli_unique_keys.add(rule_match.cli_unique_key)
        findings[rule].add(rule_match)

    # Sort results so as to guarantee the same results across different
    # runs. Results may arrive in a different order due to parallelism
    # (-j option).
    return {rule: sorted(matches) for rule, matches in findings.items()}
