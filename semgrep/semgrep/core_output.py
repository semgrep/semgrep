"""
This file encapsulates classes necessary in parsing semgrep-core
json output into a typed object

The precise type of the response from semgrep-core is specified in
https://github.com/returntocorp/semgrep/blob/develop/interfaces/Output_from_core.atd
"""
from dataclasses import replace
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import semgrep.output_from_core as core
from semgrep.error import LegacySpan
from semgrep.error import Level
from semgrep.error import SemgrepCoreError
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchSet
from semgrep.types import JsonObject
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def core_error_to_semgrep_error(err: core.Error) -> SemgrepCoreError:
    final_rule_id = err.rule_id

    # Hackily convert the level string to Semgrep expectations
    level_str = err.severity.kind
    if level_str.upper() == "WARNING":
        level_str = "WARN"
    if level_str.upper() == "ERROR_":
        level_str = "ERROR"
    level = Level[level_str.upper()]

    spans: Optional[Tuple[LegacySpan, ...]] = None
    if err.yaml_path:
        yaml_path = tuple(err.yaml_path[::-1])
        spans = tuple([LegacySpan(err.location.start, err.location.end, yaml_path)])  # type: ignore

    # TODO benchmarking code relies on error code value right now
    # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
    if err.error_type == "Syntax error" or err.error_type == "Lexical error":
        code = 3
        final_rule_id = None  # Rule id not important for parse errors
    else:
        code = 2

    return SemgrepCoreError(code, level, spans, replace(err, rule_id=final_rule_id))


def parse_core_output(raw_json: JsonObject) -> core.MatchResults:
    match_results = core.MatchResults.from_json(raw_json)

    for skip in match_results.skipped_targets:
        if skip.rule_id:
            rule_info = f"rule {skip.rule_id}"
        else:
            rule_info = "all rules"
        logger.verbose(
            f"skipped '{skip.path}' [{rule_info}]: {skip.reason}: {skip.details}"
        )
    return match_results


def core_matches_to_rule_matches(
    rules: List[Rule], res: core.MatchResults
) -> Dict[Rule, List[RuleMatch]]:
    """
    Convert core_match objects into RuleMatch objects that the rest of the codebase
    interacts with.

    For now assumes that all matches encapsulated by this object are from the same rulee
    """
    rule_table = {rule.id: rule for rule in rules}

    def interpolate(text: str, metavariables: Dict[str, str]) -> str:
        """Interpolates a string with the metavariables contained in it, returning a new string"""

        # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
        for metavariable in sorted(metavariables.keys(), key=len, reverse=True):
            text = text.replace(metavariable, metavariables[metavariable])

        return text

    def read_metavariables(match: core.Match) -> Dict[str, str]:
        result = {}

        # open path and ignore non-utf8 bytes. https://stackoverflow.com/a/56441652
        with open(match.location.path, errors="replace") as fd:
            for metavariable, metavariable_data in match.extra.metavars.items():
                # Offsets are start inclusive and end exclusive
                start_offset = metavariable_data.start.offset
                end_offset = metavariable_data.end.offset
                length = end_offset - start_offset

                # TODO Also save the propagated value

                fd.seek(start_offset)
                result[metavariable] = fd.read(length)

        return result

    def convert_to_rule_match(match: core.Match) -> RuleMatch:
        rule = rule_table[match.rule_id.value]
        metavariables = read_metavariables(match)
        message = interpolate(rule.message, metavariables)
        fix = interpolate(rule.fix, metavariables) if rule.fix else None

        return RuleMatch(
            match=match,
            extra=match.extra.to_json(),
            message=message,
            metadata=rule.metadata,
            severity=rule.severity,
            fix=fix,
            fix_regex=rule.fix_regex,
        )

    # TODO: Dict[core.RuleId, RuleMatchSet]
    findings: Dict[Rule, RuleMatchSet] = {rule: RuleMatchSet() for rule in rules}
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
