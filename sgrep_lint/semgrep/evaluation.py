import collections
from pathlib import Path
from typing import Any
from typing import DefaultDict
from typing import Dict
from typing import Iterable
from typing import List
from typing import Optional
from typing import Set

from semgrep.constants import RCE_RULE_FLAG
from semgrep.rule import Rule
from semgrep.sgrep_types import BooleanRuleExpression
from semgrep.sgrep_types import OPERATORS
from semgrep.sgrep_types import pattern_names_for_operator
from semgrep.sgrep_types import PatternId
from semgrep.sgrep_types import Range
from semgrep.sgrep_types import SgrepRange
from semgrep.util import debug_print
from semgrep.util import flatten
from semgrep.util import NEED_ARBITRARY_CODE_EXEC_EXIT_CODE
from semgrep.util import print_error
from semgrep.util import print_error_exit


def _evaluate_single_expression(
    expression: BooleanRuleExpression,
    results: Dict[PatternId, List[SgrepRange]],
    ranges_left: Set[Range],
    flags: Optional[Dict[str, Any]] = None,
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
        if not flags or flags[RCE_RULE_FLAG] != True:
            print_error_exit(
                f"at least one rule needs to execute arbitrary code; this is dangerous! if you want to continue, enable the flag: {RCE_RULE_FLAG}",
                NEED_ARBITRARY_CODE_EXEC_EXIT_CODE,
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
        print_error_exit(
            f"python where expression needs boolean output but got: {output} for {where_expression}"  # type: ignore
        )
    return output == True  # type: ignore


def parse_sgrep_output(
    sgrep_findings: List[Dict[str, Any]]
) -> Dict[PatternId, List[SgrepRange]]:
    output: DefaultDict[PatternId, List[SgrepRange]] = collections.defaultdict(list)
    for finding in sgrep_findings:
        check_id = finding["check_id"]
        pattern_id = PatternId(check_id)
        output[pattern_id].append(sgrep_finding_to_range(finding))
    return dict(output)


def sgrep_finding_to_range(sgrep_finding: Dict[str, Any]) -> SgrepRange:
    metavars = sgrep_finding["extra"]["metavars"]
    return SgrepRange(
        Range(sgrep_finding["start"]["offset"], sgrep_finding["end"]["offset"]),
        {k: v["abstract_content"] for k, v in metavars.items()},
    )


def safe_relative_to(a: Path, b: Path) -> Path:
    try:
        return a.relative_to(b)
    except ValueError:
        # paths had no common prefix; not possible to relativize
        return a


def should_exclude_this_path(path: Path) -> bool:
    return any("test" in p or "example" in p for p in path.parts)


def evaluate(
    rule: Rule, results: List[Dict[str, Any]], allow_exec: bool
) -> List[Dict[str, Any]]:
    current_path = Path(".")
    output = []
    expression = rule.expression
    check_ids_to_ranges = parse_sgrep_output(
        results
    )  # TODO should run_rules handle this
    debug_print(str(check_ids_to_ranges))
    valid_ranges_to_output = evaluate_expression(
        expression, check_ids_to_ranges, flags={RCE_RULE_FLAG: allow_exec},
    )

    # only output matches which are inside these offsets!
    debug_print(f"compiled result {valid_ranges_to_output}")
    debug_print("-" * 80)
    for result in results:
        if sgrep_finding_to_range(result).range in valid_ranges_to_output:
            path_object = Path(result["path"])

            # restore the original rule ID
            result["check_id"] = rule.id
            # rewrite the path to be relative to the current working directory
            result["path"] = str(safe_relative_to(path_object, current_path))

            # restore the original message
            result["extra"]["message"] = rewrite_message_with_metavars(rule, result)

            result = transform_to_r2c_output(result)
            output.append(result)

    return output


def rewrite_message_with_metavars(rule: Rule, sgrep_result: Dict[str, Any]) -> str:
    msg_text = rule.message
    if "metavars" in sgrep_result["extra"]:
        for metavar, contents in sgrep_result["extra"]["metavars"].items():
            msg_text = msg_text.replace(metavar, contents["abstract_content"])
    return msg_text


def transform_to_r2c_output(finding: Dict[str, Any]) -> Dict[str, Any]:
    # https://docs.r2c.dev/en/latest/api/output.html does not support offset at the moment
    if "offset" in finding["start"]:
        del finding["start"]["offset"]
    if "offset" in finding["end"]:
        del finding["end"]["offset"]
    return finding


def evaluate_expression(
    expression: BooleanRuleExpression,
    results: Dict[PatternId, List[SgrepRange]],
    flags: Optional[Dict[str, Any]] = None,
) -> Set[Range]:
    ranges_left = set([x.range for x in flatten(results.values())])
    return _evaluate_expression(expression, results, ranges_left, flags)


def _evaluate_expression(
    expression: BooleanRuleExpression,
    results: Dict[PatternId, List[SgrepRange]],
    ranges_left: Set[Range],
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
                _evaluate_expression(expr, results, ranges_left.copy(), flags)
                for expr in expression.children
            ]
            ranges_left.intersection_update(flatten(evaluated_ranges))
        else:
            # chain intersection eagerly; intersect for every AND'ed child
            for expr in expression.children:
                remainining_ranges = _evaluate_expression(
                    expr, results, ranges_left.copy(), flags
                )
                ranges_left.intersection_update(remainining_ranges)

        debug_print(f"after filter `{expression.operator}`: {ranges_left}")
    else:
        assert (
            expression.children is None
        ), f"only `{pattern_names_for_operator(OPERATORS.AND_EITHER)}` or `{pattern_names_for_operator(OPERATORS.AND_ALL)}` expressions can have multiple subpatterns"
        ranges_left = _evaluate_single_expression(
            expression, results, ranges_left, flags
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
