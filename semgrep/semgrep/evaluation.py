from pathlib import Path
from typing import Any
from typing import Dict
from typing import Iterable
from typing import List
from typing import Optional
from typing import Set
from typing import Tuple

from semgrep.constants import RCE_RULE_FLAG
from semgrep.error import NEED_ARBITRARY_CODE_EXEC_EXIT_CODE
from semgrep.error import SemgrepError
from semgrep.error import UnknownOperatorError
from semgrep.pattern_match import PatternMatch
from semgrep.rule import Rule
from semgrep.rule_match import RuleMatch
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import OPERATORS
from semgrep.semgrep_types import pattern_name_for_operator
from semgrep.semgrep_types import pattern_names_for_operator
from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import Range
from semgrep.util import debug_print
from semgrep.util import flatten
from semgrep.util import print_error


def _evaluate_single_expression(
    expression: BooleanRuleExpression,
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    ranges_left: Set[Range],
    steps_for_debugging: List[Dict[str, Any]],
    flags: Optional[Dict[str, Any]] = None,
) -> Set[Range]:

    assert expression.pattern_id, f"<internal error: expected pattern id: {expression}>"
    results_for_pattern = [
        x.range for x in pattern_ids_to_pattern_matches.get(expression.pattern_id, [])
    ]

    if expression.operator == OPERATORS.AND:
        # remove all ranges that don't equal the ranges for this pattern
        return ranges_left.intersection(results_for_pattern)
    elif expression.operator == OPERATORS.AND_NOT:
        # remove all ranges that DO equal the ranges for this pattern
        # difference_update = Remove all elements of another set from this set.
        output_ranges = ranges_left.difference(results_for_pattern)
        debug_print(f"after filter `{expression.operator}`: {output_ranges}")
        steps_for_debugging.append(
            {
                "filter": pattern_name_for_operator(expression.operator),
                "pattern_id": expression.pattern_id,
                "ranges": list(output_ranges),
            }
        )
        return output_ranges
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
        steps_for_debugging.append(
            {
                "filter": pattern_name_for_operator(expression.operator),
                "pattern_id": expression.pattern_id,
                "ranges": list(output_ranges),
            }
        )
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
        steps_for_debugging.append(
            {
                "filter": pattern_name_for_operator(expression.operator),
                "pattern_id": expression.pattern_id,
                "ranges": list(output_ranges),
            }
        )
        return output_ranges
    elif expression.operator == OPERATORS.WHERE_PYTHON:
        if not flags or flags[RCE_RULE_FLAG] != True:
            raise SemgrepError(
                f"at least one rule needs to execute arbitrary code; this is dangerous! if you want to continue, enable the flag: {RCE_RULE_FLAG}",
                code=NEED_ARBITRARY_CODE_EXEC_EXIT_CODE,
            )
        assert expression.operand, "must have operand for this operator type"

        output_ranges = set()
        # Look through every range that hasn't been filtered yet
        for pattern_match in list(flatten(pattern_ids_to_pattern_matches.values())):
            # Only need to check where-python clause if the range hasn't already been filtered

            if pattern_match.range in ranges_left:
                debug_print(
                    f"WHERE is {expression.operand}, metavars: {pattern_match.metavars}"
                )
                if _where_python_statement_matches(
                    expression.operand, pattern_match.metavars
                ):
                    output_ranges.add(pattern_match.range)
        debug_print(f"after filter `{expression.operator}`: {output_ranges}")
        steps_for_debugging.append(
            {
                "filter": pattern_name_for_operator(expression.operator),
                "pattern_id": expression.pattern_id,
                "ranges": list(output_ranges),
            }
        )
        return output_ranges
    elif expression.operator == OPERATORS.REGEX:
        # remove all ranges that don't equal the ranges for this pattern
        output_ranges = ranges_left.intersection(results_for_pattern)
        debug_print(f"after filter `{expression.operator}`: {output_ranges}")
        steps_for_debugging.append(
            {
                "filter": pattern_name_for_operator(expression.operator),
                "pattern_id": expression.pattern_id,
                "ranges": list(output_ranges),
            }
        )
        return output_ranges
    else:
        raise UnknownOperatorError(f"unknown operator {expression.operator}")


# Given a `where-python` expression as a string and currently matched metavars,
# return whether the expression matches as a boolean
def _where_python_statement_matches(
    where_expression: str, metavars: Dict[str, Any]
) -> bool:
    # TODO: filter out obvious dangerous things here
    global output
    output = None  # type: ignore

    # HACK: we're executing arbitrary Python in the where-python,
    # be careful my friend
    vars = {k: v["abstract_content"] for k, v in metavars.items()}
    try:
        exec(f"global output; output = {where_expression}")
    except Exception as ex:
        print_error(
            f"error evaluating a where-python expression: `{where_expression}`: {ex}"
        )

    if type(output) != type(True):  # type: ignore
        raise SemgrepError(
            f"python where expression needs boolean output but got: {output} for {where_expression}"  # type: ignore
        )
    return output == True  # type: ignore


def group_by_pattern_id(
    pattern_matches: List[PatternMatch],
) -> Dict[PatternId, List[PatternMatch]]:
    by_id: Dict[PatternId, List[PatternMatch]] = {}
    for pattern_match in pattern_matches:
        by_id.setdefault(pattern_match.id, []).append(pattern_match)
    return by_id


def safe_relative_to(a: Path, b: Path) -> Path:
    try:
        return a.relative_to(b)
    except ValueError:
        # paths had no common prefix; not possible to relativize
        return a


def should_exclude_this_path(path: Path) -> bool:
    return any("test" in p or "example" in p for p in path.parts)


def evaluate(
    rule: Rule, pattern_matches: List[PatternMatch], allow_exec: bool
) -> Tuple[List[RuleMatch], List[Dict[str, Any]]]:
    """
        Takes a Rule and list of pattern matches from a single file and
        handles the boolean expression evaluation of the Rule's patterns
        Returns a list of RuleMatches.
    """
    output = []

    pattern_ids_to_pattern_matches = group_by_pattern_id(pattern_matches)
    steps_for_debugging = [
        {
            "filter": "initial",
            "pattern_id": None,
            "ranges": {
                k: list(set(vv.range for vv in v))
                for k, v in pattern_ids_to_pattern_matches.items()
            },
        }
    ]
    debug_print(str(pattern_ids_to_pattern_matches))
    valid_ranges_to_output = evaluate_expression(
        rule.expression,
        pattern_ids_to_pattern_matches,
        flags={RCE_RULE_FLAG: allow_exec},
        steps_for_debugging=steps_for_debugging,
    )

    # only output matches which are inside these offsets!
    debug_print(f"compiled result {valid_ranges_to_output}")
    debug_print("-" * 80)

    for pattern_match in pattern_matches:
        if pattern_match.range in valid_ranges_to_output:
            message = interpolate_message_metavariables(rule, pattern_match)
            fix = interpolate_fix_metavariables(rule, pattern_match)
            rule_match = RuleMatch(
                rule.id,
                pattern_match,
                message=message,
                metadata=rule.metadata,
                severity=rule.severity,
                fix=fix,
            )
            output.append(rule_match)

    return output, steps_for_debugging


def interpolate_message_metavariables(rule: Rule, pattern_match: PatternMatch) -> str:
    msg_text = rule.message
    for metavar, contents in pattern_match.metavars.items():
        msg_text = msg_text.replace(metavar, contents.get("abstract_content", ""))
    return msg_text


def interpolate_fix_metavariables(
    rule: Rule, pattern_match: PatternMatch
) -> Optional[str]:
    fix_str = rule.fix
    if fix_str is None:
        return None
    for metavar, contents in pattern_match.metavars.items():
        fix_str = fix_str.replace(metavar, contents.get("abstract_content", ""))
    return fix_str


def evaluate_expression(
    expression: BooleanRuleExpression,
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    steps_for_debugging: List[Dict[str, Any]],
    flags: Optional[Dict[str, Any]] = None,
) -> Set[Range]:
    ranges_left = set(
        [x.range for x in flatten(pattern_ids_to_pattern_matches.values())]
    )
    return _evaluate_expression(
        expression,
        pattern_ids_to_pattern_matches,
        ranges_left,
        steps_for_debugging,
        flags=flags,
    )


def _evaluate_expression(
    expression: BooleanRuleExpression,
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    ranges_left: Set[Range],
    steps_for_debugging: List[Dict[str, Any]],
    flags: Optional[Dict[str, Any]] = None,
) -> Set[Range]:
    if (
        expression.operator == OPERATORS.AND_EITHER
        or expression.operator == OPERATORS.AND_ALL
    ):
        assert (
            expression.children is not None
        ), f"{pattern_names_for_operator(OPERATORS.AND_EITHER)} or {pattern_names_for_operator(OPERATORS.AND_ALL)} must have a list of subpatterns"

        # recurse on the nested expressions
        if expression.operator == OPERATORS.AND_EITHER:
            # remove anything that does not equal one of these ranges
            evaluated_ranges = [
                _evaluate_expression(
                    expr,
                    pattern_ids_to_pattern_matches,
                    ranges_left.copy(),
                    steps_for_debugging,
                    flags=flags,
                )
                for expr in expression.children
            ]
            ranges_left.intersection_update(flatten(evaluated_ranges))
        else:
            # chain intersection eagerly; intersect for every AND'ed child
            for expr in expression.children:
                remainining_ranges = _evaluate_expression(
                    expr,
                    pattern_ids_to_pattern_matches,
                    ranges_left.copy(),
                    steps_for_debugging,
                    flags=flags,
                )
                ranges_left.intersection_update(remainining_ranges)

        debug_print(f"after filter `{expression.operator}`: {ranges_left}")
        steps_for_debugging.append(
            {
                "filter": f"{pattern_name_for_operator(expression.operator)}",
                "pattern_id": None,
                "ranges": list(ranges_left),
            }
        )
    else:
        assert (
            expression.children is None
        ), f"only `{pattern_names_for_operator(OPERATORS.AND_EITHER)}` or `{pattern_names_for_operator(OPERATORS.AND_ALL)}` expressions can have multiple subpatterns"
        ranges_left = _evaluate_single_expression(
            expression,
            pattern_ids_to_pattern_matches,
            ranges_left,
            steps_for_debugging,
            flags=flags,
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
