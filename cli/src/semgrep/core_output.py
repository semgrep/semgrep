"""
This file encapsulates classes necessary in parsing semgrep-core
json output into a typed object.

Not everything is done here though; Some of the parsing
of semgrep-core output is done in core_runner.py (e.g.,
parsing and interpreting the semgrep-core profiling information).

The precise type of the response from semgrep-core is specified in
semgrep_interfaces/semgrep_output_v1.atd
"""
import copy
import dataclasses
from dataclasses import replace
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
import semgrep.util as util
from semgrep.error import FATAL_EXIT_CODE
from semgrep.error import OK_EXIT_CODE
from semgrep.error import SemgrepCoreError
from semgrep.error import TARGET_PARSE_FAILURE_EXIT_CODE
from semgrep.rule import Rule
from semgrep.rule_match import CliUniqueKey
from semgrep.rule_match import RuleMatch
from semgrep.rule_match import RuleMatchSet
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def _core_location_to_error_span(location: out.Location) -> out.ErrorSpan:
    return out.ErrorSpan(
        file=location.path,
        start=location.start,
        end=location.end,
    )


def core_error_to_semgrep_error(err: out.CoreError) -> SemgrepCoreError:
    level = err.severity
    spans: Optional[List[out.ErrorSpan]] = None
    if isinstance(err.error_type.value, out.PatternParseError):
        yaml_path = err.error_type.value.value[::-1]
        error_span = _core_location_to_error_span(err.location)
        config_start = out.Position(line=0, col=1, offset=-1)
        config_end = out.Position(
            line=err.location.end.line - err.location.start.line,
            col=err.location.end.col - err.location.start.col + 1,
            offset=-1,
        )
        spans = [
            dataclasses.replace(
                error_span,
                config_start=config_start,
                config_end=config_end,
                config_path=yaml_path,
            )
        ]
    elif isinstance(err.error_type.value, out.PartialParsing):
        # The spans for PartialParsing errors are contained in the "error_type" object
        spans = [
            _core_location_to_error_span(location)
            for location in err.error_type.value.value
        ]

    # TODO benchmarking code relies on error code value right now
    # See https://semgrep.dev/docs/cli-usage/ for meaning of codes
    if isinstance(level.value, out.Info_):
        code = OK_EXIT_CODE
    elif (
        isinstance(err.error_type.value, out.ParseError)
        or isinstance(err.error_type.value, out.LexicalError)
        or isinstance(err.error_type.value, out.PartialParsing)
    ):
        code = TARGET_PARSE_FAILURE_EXIT_CODE
        err = replace(err, rule_id=None)  # Rule id not important for parse errors
    elif isinstance(err.error_type.value, out.PatternParseError):
        # TODO This should probably be RULE_PARSE_FAILURE_EXIT_CODE
        # but we have been exiting with FATAL_EXIT_CODE, so we need
        # to be deliberate about changing it
        code = FATAL_EXIT_CODE
    else:
        code = FATAL_EXIT_CODE

    return SemgrepCoreError(code, level, spans, err)


def core_matches_to_rule_matches(
    rules: List[Rule], res: out.CoreOutput
) -> Dict[Rule, List[RuleMatch]]:
    """
    Convert core_match objects into RuleMatch objects that the rest of the codebase
    interacts with.

    For now assumes that all matches encapsulated by this object are from the same rule
    """
    rule_table = {rule.id: rule for rule in rules}

    def interpolate(
        text: str,
        metavariables: Dict[str, str],
        propagated_values: Dict[str, str],
        mask_metavariables: bool,
    ) -> str:
        """Interpolates a string with the metavariables contained in it, returning a new string"""
        if mask_metavariables:
            for metavariable in metavariables.keys():
                metavariable_content = metavariables[metavariable]
                show_until = int(len(metavariable_content) * util.MASK_SHOW_PCT)
                masked_content = metavariable_content[:show_until] + util.MASK_CHAR * (
                    len(metavariable_content) - show_until
                )
                metavariables[metavariable] = masked_content

                metavariable_value = propagated_values[metavariable]
                show_until = int(len(metavariable_content) * util.MASK_SHOW_PCT)
                masked_value = metavariable_value[:show_until] + util.MASK_CHAR * (
                    len(metavariable_content) - show_until
                )
                propagated_values[metavariable] = masked_value

        # Sort by metavariable length to avoid name collisions (eg. $X2 must be handled before $X)
        for metavariable in sorted(metavariables.keys(), key=len, reverse=True):
            text = text.replace(
                "value(" + metavariable + ")", propagated_values[metavariable]
            )
            text = text.replace(metavariable, metavariables[metavariable])

        return text

    def read_metavariables(
        match: out.CoreMatch,
    ) -> Tuple[Dict[str, str], Dict[str, str]]:
        matched_values = {}
        propagated_values = {}

        # open path and ignore non-utf8 bytes. https://stackoverflow.com/a/56441652
        with open(match.path.value, errors="replace") as fd:
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

    def convert_to_rule_match(match: out.CoreMatch) -> RuleMatch:
        rule = rule_table[match.check_id.value]
        matched_values, propagated_values = read_metavariables(match)

        message = match.extra.message if match.extra.message else rule.message
        message = interpolate(
            message,
            matched_values,
            propagated_values,
            isinstance(rule.product.value, out.Secrets),
        )

        metadata = rule.metadata
        if match.extra.metadata:
            metadata = copy.deepcopy(metadata)
            metadata.update(match.extra.metadata.value)

        if match.extra.fix is not None:
            fix = match.extra.fix
        else:
            fix = None

        return RuleMatch(
            match=match,
            extra=match.extra.to_json(),
            message=message,
            metadata=metadata,
            severity=match.extra.severity if match.extra.severity else rule.severity,
            fix=fix,
        )

    by_unique_key: Dict[CliUniqueKey, RuleMatch] = {}
    for match in res.results:
        rule_match = convert_to_rule_match(match)
        curr = by_unique_key.setdefault(rule_match.cli_unique_key, rule_match)
        if rule_match.should_report_instead(curr):
            by_unique_key[rule_match.cli_unique_key] = rule_match

    # TODO: Dict[out.RuleId, RuleMatchSet]
    findings: Dict[Rule, RuleMatchSet] = {rule: RuleMatchSet(rule) for rule in rules}
    for rule_match in by_unique_key.values():
        rule = rule_table[rule_match.rule_id]
        findings[rule].add(rule_match)

    # Sort results so as to guarantee the same results across different
    # runs. Results may arrive in a different order due to parallelism
    # (-j option).
    return {rule: sorted(matches) for rule, matches in findings.items()}
