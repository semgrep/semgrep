from typing import Any
from typing import DefaultDict
from typing import Dict
from typing import Generator
from typing import Iterable
from typing import Iterator
from typing import List
from typing import NewType
from typing import Optional
from typing import Set
from typing import Tuple

from constants import RCE_RULE_FLAG
from sgrep_types import BooleanRuleExpression
from sgrep_types import InvalidRuleSchema
from sgrep_types import Operator
from sgrep_types import operator_for_pattern_name
from sgrep_types import OPERATORS
from sgrep_types import pattern_name_for_operator
from sgrep_types import PatternId
from sgrep_types import Range
from sgrep_types import SgrepRange
from util import debug_print
from util import flatten
from util import print_error
from util import print_error_exit


def _parse_boolean_expression(
    rule_patterns: List[Dict[str, Any]], pattern_id=0, prefix=""
) -> Iterator[BooleanRuleExpression]:
    """
    Move through the expression from the YML, yielding tuples of (operator, unique-id-for-pattern, pattern)
    """
    if not isinstance(rule_patterns, list):
        raise InvalidRuleSchema(
            f"invalid type for pattern {rule_patterns}: {type(rule_patterns)} is not a list"
        )
    for pattern in rule_patterns:
        if not isinstance(pattern, dict):
            raise InvalidRuleSchema(
                f"invalid type for pattern {pattern}: {type(pattern)} is not a dict"
            )
        for boolean_operator, pattern_text in pattern.items():
            operator = operator_for_pattern_name(boolean_operator)
            if isinstance(pattern_text, list):
                sub_expression = _parse_boolean_expression(
                    pattern_text, 0, f"{prefix}.{pattern_id}"
                )
                yield BooleanRuleExpression(operator, None, list(sub_expression), None)
            elif isinstance(pattern_text, str):
                yield BooleanRuleExpression(
                    operator, PatternId(f"{prefix}.{pattern_id}"), None, pattern_text
                )
                pattern_id += 1
            else:
                raise InvalidRuleSchema(
                    f"invalid type for pattern {pattern}: {type(pattern_text)}"
                )


def build_boolean_expression(rule: Dict[str, Any]) -> Iterator[BooleanRuleExpression]:
    """
    Build a boolean expression from the yml lines in the rule

    """
    if "pattern" in rule:  # single pattern at root
        yield BooleanRuleExpression(OPERATORS.AND, rule["id"], None, rule["pattern"])
    elif "patterns" in rule:  # multiple patterns at root
        yield from _parse_boolean_expression(rule["patterns"])
    else:
        raise NotImplementedError(f"unknown operator in rule {rule}")


def _evaluate_single_expression(
    expression: BooleanRuleExpression,
    results: Dict[PatternId, List[SgrepRange]],
    ranges_left: Set[Range],
    **flags,
) -> Set[Range]:

    assert expression.pattern_id, f"<internal error: expected pattern id: {expression}>"
    results_for_pattern = [x.range for x in results.get(expression.pattern_id, [])]

    if expression.operator == OPERATORS.AND:
        # remove all ranges that don't equal the ranges for this pattern
        return ranges_left.intersection(results_for_pattern)
    elif expression.operator == OPERATORS.AND_NOT:
        # remove all ranges that DO equal the ranges for this pattern
        # difference_update = Remove all elements of another set from this set.
        return ranges_left.difference(results_for_pattern)
    elif expression.operator == OPERATORS.AND_INSIDE:
        # remove all ranges (not enclosed by) or (not equal to) the inside ranges
        output_ranges = set()
        for arange in ranges_left:
            for keep_inside_this_range in results_for_pattern:
                is_enclosed = keep_inside_this_range.is_enclosing_or_eq(arange)
                # print(
                #    f'candidate range is {arange}, needs to be `{operator}` {keep_inside_this_range}; keep?: {keep}')
                if is_enclosed:
                    output_ranges.add(arange)
                    break  # found a match, no need to keep going
        debug_print(f"after filter `{expression.operator}`: {output_ranges}")
        return output_ranges
    elif expression.operator == OPERATORS.AND_NOT_INSIDE:
        # remove all ranges enclosed by or equal to
        output_ranges = ranges_left.copy()
        for arange in ranges_left:
            for keep_inside_this_range in results_for_pattern:
                if keep_inside_this_range.is_enclosing_or_eq(arange):
                    output_ranges.remove(arange)
                    break
        debug_print(f"after filter `{expression.operator}`: {output_ranges}")
        return output_ranges
    elif expression.operator == OPERATORS.WHERE_PYTHON:
        if not RCE_RULE_FLAG not in flags:
            print_error_exit(
                f"at least one rule needs to execute arbitrary code; this is dangerous! if you want to continue, enable the flag: {RCE_RULE_FLAG}"
            )
        assert expression.operand, "must have operand for this operator type"

        output_ranges = set()
        # Look through every range that hasn't been filtered yet
        for sgrep_range in list(flatten(results.values())):
            # Only need to check where-python clause if the range hasn't already been filtered

            if sgrep_range.range in ranges_left:
                debug_print(
                    f"WHERE is {expression.operand}, metavars: {sgrep_range.metavars}"
                )
                if _where_python_statement_matches(
                    expression.operand, sgrep_range.metavars
                ):
                    output_ranges.add(sgrep_range.range)
        debug_print(f"after filter `{expression.operator}`: {output_ranges}")
        return output_ranges

    else:
        raise NotImplementedError(f"unknown operator {expression.operator}")


# Given a `where-python` expression as a string and currently matched metavars,
# return whether the expression matches as a boolean
def _where_python_statement_matches(
    where_expression: str, metavars: Dict[str, str]
) -> bool:
    # TODO: filter out obvious dangerous things here
    global output
    output = None  # type: ignore

    # HACK: we're executing arbitrary Python in the where-python,
    # be careful my friend
    vars = metavars
    try:
        exec(f"global output; output = {where_expression}")
    except Exception as ex:
        print_error(
            f"error evaluating a where-python expression: `{where_expression}`: {ex}"
        )

    if type(output) != type(True):  # type: ignore
        print_error_exit(  # type: ignore
            f"python where expression needs boolean output but got: {output} for {where_expression}"  # type: ignore
        )  # type: ignore
    return output == True  # type: ignore


def evaluate_expression(
    expression, results: Dict[PatternId, List[SgrepRange]], **flags
) -> Set[Range]:
    ranges_left = set([x.range for x in flatten(results.values())])
    return _evaluate_expression(expression, results, ranges_left, **flags)


def _evaluate_expression(
    expressions: List[BooleanRuleExpression],
    results: Dict[PatternId, List[SgrepRange]],
    ranges_left: Set[Range],
    **flags,
) -> Set[Range]:
    for expression in expressions:
        if (
            expression.operator == OPERATORS.AND_EITHER
            or expression.operator == OPERATORS.AND_ALL
        ):
            assert (
                expression.children is not None
            ), f"{pattern_name_for_operator(OPERATORS.AND_EITHER)} or {pattern_name_for_operator(OPERATORS.AND_ALL)} must have a list of subpatterns"

            # recurse on the nested expressions
            evaluated_ranges = [
                _evaluate_expression([expr], results, ranges_left.copy())
                for expr in expression.children
            ]
            debug_print(
                f"recursion result {evaluated_ranges} (flat: {list(flatten(evaluated_ranges))}))"
            )

            if expression.operator == OPERATORS.AND_EITHER:
                # remove anything that does not equal one of these ranges
                ranges_left.intersection_update(flatten(evaluated_ranges))
            elif expression.operator == OPERATORS.AND_ALL:
                # chain intersection of every range returned
                for arange in evaluated_ranges:
                    ranges_left.intersection_update(arange)
            debug_print(f"after filter `{expression.operator}`: {ranges_left}")
        else:
            assert (
                expression.children is None
            ), f"only `{pattern_name_for_operator(OPERATORS.AND_EITHER)}` or `{pattern_name_for_operator(OPERATORS.AND_ALL)}` expressions can have multiple subpatterns"
            ranges_left = _evaluate_single_expression(
                expression, results, ranges_left, **flags
            )
    return ranges_left


def enumerate_patterns_in_boolean_expression(
    expressions: Iterable[BooleanRuleExpression],
) -> Iterable[BooleanRuleExpression]:
    """
    flatten a potentially nested expression
    """
    for expr in expressions:
        if expr.children is not None:
            # we need to preserve this parent of multiple children, but it has no corresponding pattern
            yield BooleanRuleExpression(expr.operator, None, None, None)
            # now yield all the children
            yield from enumerate_patterns_in_boolean_expression(expr.children)
        else:
            yield expr
