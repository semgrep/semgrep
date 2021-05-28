import copy
import re
from collections import defaultdict
from collections import OrderedDict
from typing import Any
from typing import Callable
from typing import Dict
from typing import Iterable
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple
from typing import Union

import attr

from semgrep.constants import BREAK_LINE
from semgrep.constants import RCE_RULE_FLAG
from semgrep.error import NEED_ARBITRARY_CODE_EXEC_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.error import UnknownOperatorError
from semgrep.metavariable_comparison import metavariable_comparison
from semgrep.pattern_match import PatternMatch
from semgrep.rule import Rule
from semgrep.rule_lang import YamlMap
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import OPERATORS
from semgrep.semgrep_types import OPERATORS_WITH_CHILDREN
from semgrep.semgrep_types import pattern_name_for_operator
from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import Range
from semgrep.semgrep_types import TAINT_MODE
from semgrep.util import flatten
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

DebugRanges = Union[Set[Range], Dict[str, Set[Range]]]
DebugRangesConverted = Union[List[Range], Dict[str, List[Range]]]


def stabilize_evaluation_ordering(
    iterable: Iterable, key: Optional[Callable] = None
) -> Iterable:
    # Set reverse=True so we find the "nearest" pattern match or range, looking
    # backwards. That is, if we have nested functions, classes, or other code blocks,
    # we will produce the innermost block as the finding context
    return sorted(iterable, key=key, reverse=True)


def convert_ranges(ranges: DebugRanges) -> DebugRangesConverted:
    # Make ranges JSON serializable and sort for performing comparisons
    return (
        sorted(list(ranges))
        if isinstance(ranges, set)
        else {k: sorted(list(v)) for k, v in ranges.items()}
    )


@attr.s(auto_attribs=True, frozen=True)
class DebuggingStep:
    filter: str
    pattern_id: Optional[str]
    ranges: DebugRangesConverted = attr.ib(converter=convert_ranges)
    metavar_ranges: Dict[str, List[PatternMatch]]


def add_debugging_info(
    expression: BooleanRuleExpression,
    output_ranges: Set[Range],
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    steps_for_debugging: List[DebuggingStep],
) -> None:

    metavars_for_patterns: Dict[str, List[PatternMatch]] = defaultdict(list)
    if expression.pattern_id is None:
        metavars_for_patterns = {}
    else:
        for pattern in pattern_ids_to_pattern_matches.get(expression.pattern_id, []):
            for metavar, metavar_values in pattern.metavariables.items():
                metavars_for_patterns[metavar].append(metavar_values)

    logger.debug(f"after filter '{expression.operator}': {output_ranges}")

    steps_for_debugging.append(
        DebuggingStep(
            pattern_name_for_operator(expression.operator),
            expression.pattern_id,
            output_ranges,
            metavars_for_patterns,
        )
    )


def compare_propagated_metavariable(
    _range: Range,
    pattern_match: PatternMatch,
    metavariable: str,
) -> bool:

    return (
        metavariable in _range.propagated_metavariables
        and metavariable in pattern_match.metavariable_uids
        and _range.propagated_metavariables[metavariable]
        == pattern_match.metavariable_uids[metavariable]
    )


def get_re_range_matches(
    metavariable: str,
    regex: str,
    ranges: Set[Range],
    pattern_matches: List[PatternMatch],
) -> Set[Range]:

    result: Set[Range] = set()
    for _range in ranges:
        if (
            metavariable not in _range.metavariables
            and metavariable not in _range.propagated_metavariables
        ):
            logger.debug(f"metavariable '{metavariable}' missing in range '{_range}'")
            continue

        any_matching_ranges = any(
            pm.range == _range
            and metavariable in pm.metavariables
            and re.match(regex, pm.get_metavariable_value(metavariable))
            for pm in pattern_matches
        )
        any_propagated_ranges = any(
            re.match(regex, pm.get_metavariable_value(metavariable))
            for pm in pattern_matches
            if compare_propagated_metavariable(_range, pm, metavariable)
        )
        if any_matching_ranges or any_propagated_ranges:
            result.add(_range)

    return result


def compare_range_match(
    metavariable: str,
    comparison: str,
    strip: Optional[bool],
    base: Optional[int],
    content: str,
) -> bool:

    if strip:
        content = content.strip("\"'`")

    try:
        # Assume float data if "." in content
        if "." in content:
            converted = float(content)
        else:
            if base is not None:
                converted = int(content, base=base)
            else:
                converted = int(content)
    except ValueError:
        logger.debug(
            f"metavariable '{metavariable}' incorrect comparison type '{content}'"
        )
        return False

    return metavariable_comparison(metavariable, comparison, converted)


def get_comparison_range_matches(
    metavariable: str,
    comparison: str,
    strip: Optional[bool],
    base: Optional[int],
    ranges: Set[Range],
    pattern_matches: List[PatternMatch],
) -> Set[Range]:

    result: Set[Range] = set()
    for _range in ranges:
        if (
            metavariable not in _range.metavariables
            and metavariable not in _range.propagated_metavariables
        ):
            logger.debug(f"metavariable '{metavariable}' missing in range '{_range}'")
            continue

        any_matching_ranges = any(
            pm.range == _range
            and metavariable in pm.metavariables
            and compare_range_match(
                metavariable,
                comparison,
                strip,
                base,
                pm.get_metavariable_value(metavariable),
            )
            for pm in pattern_matches
        )
        any_propagated_ranges = any(
            compare_range_match(
                metavariable,
                comparison,
                strip,
                base,
                pm.get_metavariable_value(metavariable),
            )
            for pm in pattern_matches
            if compare_propagated_metavariable(_range, pm, metavariable)
        )
        if any_matching_ranges or any_propagated_ranges:
            result.add(_range)

    return result


def compare_where_python(where_expression: str, pattern_match: PatternMatch) -> bool:
    result = False
    return_var = "semgrep_pattern_return"
    lines = where_expression.strip().split("\n")
    to_eval = "\n".join(lines[:-1] + [f"{return_var} = {lines[-1]}"])

    local_vars = {
        metavar: pattern_match.get_metavariable_value(metavar)
        for metavar in pattern_match.metavariables
    }
    scope = {"vars": local_vars}

    try:
        # fmt: off
        exec(to_eval, scope)  # nosem: contrib.dlint.dlint-equivalent.insecure-exec-use, python.lang.security.audit.exec-detected.exec-detected
        # fmt: on
        result = scope[return_var]  # type: ignore
    except KeyError as ex:
        logger.error(
            f"could not find metavariable {ex} while evaluating where-python expression '{where_expression}', consider case where metavariable is missing"
        )
    except Exception as ex:
        logger.error(
            f"received error '{repr(ex)}' while evaluating where-python expression '{where_expression}'"
        )

    if not isinstance(result, bool):
        raise SemgrepError(
            f"where-python expression '{where_expression}' needs boolean output but got {result}"
        )
    return result


def get_where_python_range_matches(
    where_expression: str, ranges: Set[Range], pattern_matches: List[PatternMatch]
) -> Set[Range]:

    return {
        pm.range
        for pm in pattern_matches
        if pm.range in ranges and compare_where_python(where_expression, pm)
    }


def filter_ranges_with_propagation(
    ranges_left: Set[Range],
    ranges_for_pattern: Set[Range],
    predicate: Callable[[Range, Range], bool],
    metavariable_propagation: bool,
) -> Set[Range]:

    stabilized_ranges_left = stabilize_evaluation_ordering(ranges_left)
    stabilized_ranges_for_pattern = stabilize_evaluation_ordering(ranges_for_pattern)
    result: Set[Range] = set()
    for _range in stabilized_ranges_left:
        matched = False
        for pattern_range in stabilized_ranges_for_pattern:
            if predicate(pattern_range, _range):
                if metavariable_propagation:
                    # We need a deepcopy here because Python sets are passed by reference,
                    # and the call to update() will affect ranges referenced in other
                    # recursive calls to _evaluate_expression
                    new_range = copy.deepcopy(_range)
                    new_range.propagated_metavariables.update(
                        pattern_range.metavariables
                    )
                else:
                    new_range = _range
                matched = True
        if matched:
            result.add(new_range)
    return result


def _evaluate_single_expression(
    expression: BooleanRuleExpression,
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    ranges_left: Set[Range],
    allow_exec: bool,
    metavariable_propagation: bool,
) -> Set[Range]:

    if not expression.pattern_id:
        raise SemgrepError(f"expected expression '{expression}' to have pattern_id")

    ranges_for_pattern = {
        x.range for x in pattern_ids_to_pattern_matches.get(expression.pattern_id, [])
    }

    if expression.operator == OPERATORS.AND:
        # remove all ranges that don't equal the ranges for this pattern
        output_ranges = filter_ranges_with_propagation(
            ranges_left,
            ranges_for_pattern,
            predicate=lambda r1, r2: r1 == r2,
            metavariable_propagation=metavariable_propagation,
        )
    elif expression.operator == OPERATORS.AND_NOT:
        # remove all ranges that DO equal the ranges for this pattern
        output_ranges = ranges_left.difference(ranges_for_pattern)
    elif expression.operator == OPERATORS.AND_INSIDE:
        # remove all ranges (not enclosed by) or (not equal to) the inside ranges
        output_ranges = filter_ranges_with_propagation(
            ranges_left,
            ranges_for_pattern,
            predicate=lambda r1, r2: r1.is_enclosing_or_eq(r2),
            metavariable_propagation=metavariable_propagation,
        )
    elif expression.operator == OPERATORS.AND_NOT_INSIDE:
        # remove all ranges enclosed by or equal to
        output_ranges = {
            _range
            for _range in ranges_left
            if not any(
                pattern_range.is_enclosing_or_eq(_range)
                for pattern_range in ranges_for_pattern
            )
        }
    elif expression.operator == OPERATORS.REGEX:
        # remove all ranges that don't equal the ranges for this pattern
        output_ranges = ranges_left.intersection(ranges_for_pattern)
    elif expression.operator == OPERATORS.NOT_REGEX:
        # remove the result if pattern-not-regex is within another pattern
        output_ranges = {
            _range
            for _range in ranges_left
            if not any(
                _range.is_range_enclosing_or_eq(pattern_range)
                or pattern_range.is_range_enclosing_or_eq(_range)
                for pattern_range in ranges_for_pattern
            )
        }
    elif expression.operator == OPERATORS.WHERE_PYTHON:
        if not allow_exec:
            raise SemgrepError(
                f"at least one rule needs to execute arbitrary code; this is dangerous! if you want to continue, enable the flag: {RCE_RULE_FLAG}",
                code=NEED_ARBITRARY_CODE_EXEC_EXIT_CODE,
            )
        if not isinstance(expression.operand, str):
            raise SemgrepError(
                f"expected operator '{expression.operator}' to have string value guaranteed by schema"
            )
        output_ranges = get_where_python_range_matches(
            expression.operand,
            ranges_left,
            list(flatten(pattern_ids_to_pattern_matches.values())),
        )

    elif expression.operator == OPERATORS.METAVARIABLE_REGEX:
        if not isinstance(expression.operand, YamlMap):
            raise SemgrepError(
                f"expected operator '{expression.operator}' to have mapping value guaranteed by schema"
            )
        output_ranges = get_re_range_matches(
            expression.operand["metavariable"].value,
            expression.operand["regex"].value,
            ranges_left,
            list(flatten(pattern_ids_to_pattern_matches.values())),
        )
    elif expression.operator == OPERATORS.METAVARIABLE_COMPARISON:
        if not isinstance(expression.operand, YamlMap):
            raise SemgrepError(
                f"expected operator '{expression.operator}' to have mapping value guaranteed by schema"
            )
        strip = expression.operand.get("strip")
        base = expression.operand.get("base")
        output_ranges = get_comparison_range_matches(
            expression.operand["metavariable"].value,
            expression.operand["comparison"].value,
            strip.value if strip is not None else None,
            base.value if base is not None else None,
            ranges_left,
            list(flatten(pattern_ids_to_pattern_matches.values())),
        )
    else:
        raise UnknownOperatorError(f"unknown operator {expression.operator}")

    return output_ranges


def create_output(
    rule: Rule,
    pattern_matches: List[PatternMatch],
    valid_ranges_to_output: Optional[Set[Range]] = None,
) -> List[RuleMatch]:
    output = []

    if valid_ranges_to_output is None:
        valid_ranges_to_output = {
            pattern_match.range for pattern_match in pattern_matches
        }

    propagated_metavariable_lookup = {
        _range: {
            metavariable: pm.get_metavariable_value(metavariable)
            for pm in pattern_matches
            for metavariable in _range.propagated_metavariables
            if compare_propagated_metavariable(_range, pm, metavariable)
        }
        for _range in valid_ranges_to_output
    }

    for pattern_match in pattern_matches:
        if pattern_match.range in valid_ranges_to_output:
            propagated_metavariables = propagated_metavariable_lookup[
                pattern_match.range
            ]
            message = interpolate_message_metavariables(
                rule, pattern_match, propagated_metavariables
            )
            fix = interpolate_fix_metavariables(
                rule, pattern_match, propagated_metavariables
            )
            rule_match = RuleMatch.from_pattern_match(
                rule.id,
                pattern_match,
                message=message,
                metadata=rule.metadata,
                severity=rule.severity,
                fix=fix,
                fix_regex=rule.fix_regex,
            )
            output.append(rule_match)

    return sorted(output, key=lambda rule_match: rule_match._pattern_match.range.start)


def evaluate(
    rule: Rule, pattern_matches: List[PatternMatch], allow_exec: bool
) -> Tuple[List[RuleMatch], List[Dict[str, Any]]]:
    """
    Takes a Rule and list of pattern matches from a single file and
    handles the boolean expression evaluation of the Rule's patterns
    Returns a list of RuleMatches.
    """
    output = []

    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]] = OrderedDict()
    for pm in stabilize_evaluation_ordering(pattern_matches, key=lambda pm: pm.id):
        pattern_ids_to_pattern_matches.setdefault(pm.id, []).append(pm)

    initial_ranges: DebugRanges = {
        pattern_id: set(pm.range for pm in pattern_matches)
        for pattern_id, pattern_matches in pattern_ids_to_pattern_matches.items()
    }
    steps_for_debugging = [DebuggingStep("initial", None, initial_ranges, {})]

    if rule.mode == TAINT_MODE:
        valid_ranges_to_output = {
            pattern_match.range for pattern_match in pattern_matches
        }
    else:
        valid_ranges_to_output = evaluate_expression(
            rule.expression,
            pattern_ids_to_pattern_matches,
            allow_exec=allow_exec,
            steps_for_debugging=steps_for_debugging,
        )

        # only output matches which are inside these offsets!
        logger.debug(f"compiled result {valid_ranges_to_output}")
        logger.debug(BREAK_LINE)

    output = create_output(rule, pattern_matches, valid_ranges_to_output)

    return output, [attr.asdict(step) for step in steps_for_debugging]


def interpolate_message_metavariables(
    rule: Rule, pattern_match: PatternMatch, propagated_metavariables: Dict[str, str]
) -> str:
    msg_text = rule.message
    for metavariable in pattern_match.metavariables:
        msg_text = msg_text.replace(
            metavariable, pattern_match.get_metavariable_value(metavariable)
        )
    for metavariable, metavariable_text in propagated_metavariables.items():
        msg_text = msg_text.replace(metavariable, metavariable_text)
    return msg_text


def interpolate_fix_metavariables(
    rule: Rule, pattern_match: PatternMatch, propagated_metavariables: Dict[str, str]
) -> Optional[str]:
    fix_str = rule.fix
    if fix_str is None:
        return None
    for metavariable in pattern_match.metavariables:
        fix_str = fix_str.replace(
            metavariable, pattern_match.get_metavariable_value(metavariable)
        )
    for metavariable, metavariable_text in propagated_metavariables.items():
        fix_str = fix_str.replace(metavariable, metavariable_text)
    return fix_str


def evaluate_expression(
    expression: BooleanRuleExpression,
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    steps_for_debugging: List[DebuggingStep],
    allow_exec: bool,
) -> Set[Range]:
    ranges_left = {x.range for x in flatten(pattern_ids_to_pattern_matches.values())}
    return _evaluate_expression(
        expression,
        pattern_ids_to_pattern_matches,
        ranges_left,
        steps_for_debugging,
        allow_exec=allow_exec,
        metavariable_propagation=False,
    )


def _evaluate_expression(
    expression: BooleanRuleExpression,
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    ranges_left: Set[Range],
    steps_for_debugging: List[DebuggingStep],
    allow_exec: bool,
    metavariable_propagation: bool,
) -> Set[Range]:
    if expression.operator in OPERATORS_WITH_CHILDREN:
        if expression.children is None:
            raise SemgrepError(
                f"operator '{expression.operator}' must have child operators"
            )

        # recurse on the nested expressions
        if expression.operator == OPERATORS.AND_EITHER:
            # remove anything that does not equal one of these ranges
            evaluated_ranges = [
                _evaluate_expression(
                    expr,
                    pattern_ids_to_pattern_matches,
                    ranges_left.copy(),
                    steps_for_debugging,
                    allow_exec=allow_exec,
                    metavariable_propagation=False,
                )
                for expr in expression.children
            ]
            ranges_left.intersection_update(flatten(evaluated_ranges))
        elif expression.operator == OPERATORS.AND_ALL:
            # chain intersection eagerly; intersect for every AND'ed child
            for expr in expression.children:
                remainining_ranges = _evaluate_expression(
                    expr,
                    pattern_ids_to_pattern_matches,
                    ranges_left.copy(),
                    steps_for_debugging,
                    allow_exec=allow_exec,
                    metavariable_propagation=True,
                )
                ranges_left.intersection_update(remainining_ranges)
        else:
            raise UnknownOperatorError(f"unknown operator {expression.operator}")
    else:
        if expression.children is not None:
            raise SemgrepError(
                f"operator '{expression.operator}' must not have child operators"
            )

        ranges_left = _evaluate_single_expression(
            expression,
            pattern_ids_to_pattern_matches,
            ranges_left,
            allow_exec=allow_exec,
            metavariable_propagation=metavariable_propagation,
        )

    add_debugging_info(
        expression,
        ranges_left,
        pattern_ids_to_pattern_matches,
        steps_for_debugging,
    )
    return ranges_left


def enumerate_patterns_in_boolean_expression(
    expr: BooleanRuleExpression,
) -> Iterable[BooleanRuleExpression]:
    """
    flatten a potentially nested expression
    """
    if expr.children is not None:
        # we need to preserve this parent of multiple children, but it has no corresponding pattern
        yield BooleanRuleExpression(expr.operator, None, None, None)
        # now yield all the children
        for c in expr.children:
            yield from enumerate_patterns_in_boolean_expression(c)
    else:
        yield expr
