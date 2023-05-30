"""
This file encapsulates classes necessary in parsing semgrep-core
json output into a typed object

The precise type of the response from semgrep-core is specified in
https://github.com/returntocorp/semgrep/blob/develop/interfaces/Output_from_core.atd
"""
import dataclasses
from dataclasses import replace
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

import semgrep.output_from_core as core
import semgrep.semgrep_interfaces.semgrep_output_v1 as out
import semgrep.util as util
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import Level
from semgrep.error import SemgrepCoreError
from semgrep.error import SemgrepError
from semgrep.error import TARGET_PARSE_FAILURE_EXIT_CODE
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchSet
from semgrep.types import JsonObject
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def _core_location_to_error_span(location: core.Location) -> out.ErrorSpan:
    return out.ErrorSpan(
        file=location.path,
        start=out.PositionBis(line=location.start.line, col=location.start.col),
        end=out.PositionBis(line=location.end.line, col=location.end.col),
    )


def core_error_to_semgrep_error(err: core.CoreError) -> SemgrepCoreError:
    # Hackily convert the level string to Semgrep expectations
    level_str = err.severity.kind
    if level_str.upper() == "WARNING":
        level_str = "WARN"
    if level_str.upper() == "ERROR_":
        level_str = "ERROR"
    level = Level[level_str.upper()]

    spans: Optional[List[out.ErrorSpan]] = None
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
    elif isinstance(err.error_type.value, core.PartialParsing):
        # The spans for PartialParsing errors are contained in the "error_type" object
        spans = [
            _core_location_to_error_span(location)
            for location in err.error_type.value.value
        ]

    # TODO benchmarking code relies on error code value right now
    # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
    if (
        isinstance(err.error_type.value, core.ParseError)
        or isinstance(err.error_type.value, core.LexicalError)
        or isinstance(err.error_type.value, core.PartialParsing)
    ):
        code = TARGET_PARSE_FAILURE_EXIT_CODE
        err = replace(err, rule_id=None)  # Rule id not important for parse errors
    elif isinstance(err.error_type.value, core.PatternParseError):
        # TODO This should probably be RULE_PARSE_FAILURE_EXIT_CODE
        # but we have been exiting with FATAL_EXIT_CODE, so we need
        # to be deliberate about changing it
        code = FATAL_EXIT_CODE
    else:
        code = FATAL_EXIT_CODE

    return SemgrepCoreError(code, level, spans, err)


def parse_core_output(raw_json: JsonObject) -> core.CoreMatchResults:
    match_results = core.CoreMatchResults.from_json(raw_json)
    if match_results.skipped_targets:
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
    rules: List[Rule], res: core.CoreMatchResults
) -> Dict[Rule, List[RuleMatch]]:
    """
    Convert core_match objects into RuleMatch objects that the rest of the codebase
    interacts with.

    For now assumes that all matches encapsulated by this object are from the same rule
    """
    rule_table = {rule.id: rule for rule in rules}

    def interpolate(
        text: str, metavariables: Dict[str, str], propagated_values: Dict[str, str]
    ) -> str:
        """Interpolates a string with the metavariables contained in it, returning a new string"""

        # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
        for metavariable in sorted(metavariables.keys(), key=len, reverse=True):
            text = text.replace(
                "value(" + metavariable + ")", propagated_values[metavariable]
            )
            text = text.replace(metavariable, metavariables[metavariable])

        return text

    def read_metavariables(
        match: core.CoreMatch,
    ) -> Tuple[Dict[str, str], Dict[str, str]]:
        matched_values = {}
        propagated_values = {}

        # open path and ignore non-utf8 bytes. https://stackoverflow.com/a/56441652
        with open(match.location.path.value, errors="replace") as fd:
            for metavariable, metavariable_data in match.extra.metavars.value.items():
                # Offsets are start inclusive and end exclusive
                start_offset = metavariable_data.start.offset
                end_offset = metavariable_data.end.offset

                matched_value = util.read_range(fd, start_offset, end_offset)

                # Use propagated value
                if metavariable_data.propagated_value:
                    propagated_value = (
                        metavariable_data.propagated_value.svalue_abstract_content
                    )
                else:
                    propagated_value = matched_value

                matched_values[metavariable] = matched_value
                propagated_values[metavariable] = propagated_value

        return matched_values, propagated_values

    def convert_to_rule_match(match: core.CoreMatch) -> RuleMatch:
        rule = rule_table[match.rule_id.value]
        matched_values, propagated_values = read_metavariables(match)
        message = interpolate(rule.message, matched_values, propagated_values)
        if match.extra.rendered_fix is not None:
            fix = match.extra.rendered_fix
            logger.debug(f"Using AST-based autofix rendered in semgrep-core: `{fix}`")
        elif rule.fix is not None:
            fix = interpolate(rule.fix, matched_values, propagated_values)
            logger.debug(f"Using text-based autofix rendered in cli: `{fix}`")
        else:
            fix = None
        fix_regex = None

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

        return RuleMatch(
            match=match,
            extra=match.extra.to_json(),
            message=message,
            metadata=rule.metadata,
            severity=rule.severity,
            fix=fix,
            fix_regex=fix_regex,
        )

    # TODO: Dict[core.RuleId, RuleMatchSet]
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
