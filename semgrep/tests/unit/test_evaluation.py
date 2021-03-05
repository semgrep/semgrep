#!/usr/bin/env python3
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Set
from unittest.mock import MagicMock
from unittest.mock import PropertyMock

import pytest

from semgrep.error import SemgrepError
from semgrep.evaluation import enumerate_patterns_in_boolean_expression
from semgrep.evaluation import evaluate_expression as raw_evaluate_expression
from semgrep.pattern_match import PatternMatch
from semgrep.rule import Rule
from semgrep.semgrep_types import BooleanRuleExpression
from semgrep.semgrep_types import Operator
from semgrep.semgrep_types import OPERATORS
from semgrep.semgrep_types import PatternId
from semgrep.semgrep_types import Range


def evaluate_expression(
    exprs: List[BooleanRuleExpression],
    pattern_ids_to_pattern_matches: Dict[PatternId, List[PatternMatch]],
    allow_exec: bool = False,
) -> Set[Range]:
    # convert it to an implicit and
    e = BooleanRuleExpression(OPERATORS.AND_ALL, None, exprs, None)
    result: Set[Range] = raw_evaluate_expression(
        e, pattern_ids_to_pattern_matches, [], allow_exec=allow_exec
    )
    return result


def PatternMatchMock(
    start: int, end: int, metavars: Optional[Dict[str, Any]] = None
) -> PatternMatch:
    if metavars is None:
        metavars = {}

    mock = MagicMock()

    range_property = PropertyMock(return_value=Range(start, end, metavars))
    type(mock).range = range_property

    metavars_property = PropertyMock(return_value=metavars)
    type(mock).metavariables = metavars_property

    def mocked_get_metavariable_value(metavariable: str) -> Any:
        return metavars[metavariable]["abstract_content"] if metavars else ""

    mock.get_metavariable_value = mocked_get_metavariable_value

    return mock


def RuleExpr(
    operator: Operator,
    fake_pattern_name: str,
    children: Optional[List[BooleanRuleExpression]] = None,
) -> BooleanRuleExpression:
    return BooleanRuleExpression(
        operator, PatternId(fake_pattern_name), children, "fake-pattern-text-here"
    )


def testA() -> None:
    """

    TODO: what about nested booleans?

    let pattern1 = subprocess.Popen($X, safeflag=True)
    let pattern2 =  subprocess.Popen($X, ...)

        import subprocess
        subprocess.Popen(subprocess.Popen(safeFlag=True))
        ------------------------------------------------- P2        R1
                         ------------------------------- P1, P2     R2

    and-not P1 --> remove all ranges equal to P1 exactly (removes R2)
    and P2 --> remove all ranges that don't equal P2 (R1 stays)
    OUTPUT: R1

    """
    results = {
        PatternId("pattern1"): [PatternMatchMock(30, 100)],
        PatternId("pattern2"): [PatternMatchMock(0, 100), PatternMatchMock(30, 100)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_NOT, "pattern1"),
        RuleExpr(OPERATORS.AND, "pattern2"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(0, 100, {})]), f"{result}"


def testB() -> None:
    """
    Our algebra needs to express "AND-INSIDE" as opposed to "AND-NOT-INSIDE", so that cases
    like this one can still fire (we explicitly don't want to ignore the nested expression
    just because it's inside).

        let pattern1 = subprocess.Popen($X, safeflag=True)
        let pattern2 = subprocess.Popen($X, ...)
        let pattern3 = importlib.Popen($X, ...)


        import subprocess
        subprocess.Popen(subprocess.Popen(bad), safeFlag=True)
        ----------------------------------------------------- P1, P2        R1
                         ---------------------                P2            R2

        and-not P1 --> remove all ranges == P1 exactly (R1 is removed)
        and-or (P2, P3) -->  remove any ranges not exactly == P2 or P3. (R2 stays)
        OUTPUT: R2
    """
    results: Dict[PatternId, List[PatternMatch]] = {
        PatternId("pattern1"): [PatternMatchMock(0, 100)],
        PatternId("pattern2"): [PatternMatchMock(30, 70), PatternMatchMock(0, 100)],
        PatternId("pattern3"): [],
    }
    expression = [
        RuleExpr(OPERATORS.AND_NOT, "pattern1"),
        BooleanRuleExpression(
            OPERATORS.AND_EITHER,
            None,
            [RuleExpr(OPERATORS.AND, "pattern2"), RuleExpr(OPERATORS.AND, "pattern3")],
        ),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(30, 70, {})]), f"{result}"


def testC() -> None:
    """
    let pattern1 = subprocess.Popen($X, safeflag=True)
    let pattern2 = subprocess.Popen($X, ...)
    let pattern4 = def __eq__(): \n...

    def __eq__():
        import subprocess
        subprocess.Popen(subprocess.Popen(bad), safeFlag=True)
        -----------------------------------------------------   P1, P2      R1
                        ---------------------                   P2          R2
    ----------------------------------------------------------  P4          R3

    and-inside P4 --> remove all ranges that are not enclosed by P4. Now only ranges inside or equal to P4 are left (all ranges remain)
    and-not P1 --> remove all ranges == P1. Now only ranges that don't have P1 remain (R2, R3 remain)
    and P2 --> remove all ranges not == P2. R2 remains.
    OUTPUT: R2
    """
    results = {
        PatternId("pattern1"): [PatternMatchMock(100, 1000)],
        PatternId("pattern2"): [
            PatternMatchMock(100, 1000),
            PatternMatchMock(200, 300),
        ],
        PatternId("pattern4"): [PatternMatchMock(0, 1000)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_INSIDE, "pattern4"),
        RuleExpr(OPERATORS.AND_NOT, "pattern1"),
        RuleExpr(OPERATORS.AND, "pattern2"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(200, 300, {})]), f"{result}"


def testD() -> None:
    """
    let pattern1 = subprocess.Popen($X, safeflag=True)
    let pattern2 = subprocess.Popen($X, ...)
    let pattern4 = def __eq__(): \n...

    def __eq__():
        import subprocess
        subprocess.Popen(subprocess.Popen(bad), safeFlag=True)
        -----------------------------------------------------   P1, P2      R1
                        ---------------------                   P2          R2
    ----------------------------------------------------------  P4          R3

    and-not-inside P4 --> remove all ranges that are not enclosed by P4. Now only ranges inside or equal to P4 are left (no ranges remain)
    and-not P1 --> no effect
    and P2 --> no effect
    OUTPUT: []
    """
    results = {
        PatternId("pattern1"): [PatternMatchMock(100, 1000)],
        PatternId("pattern2"): [
            PatternMatchMock(100, 1000),
            PatternMatchMock(200, 300),
        ],
        PatternId("pattern4"): [PatternMatchMock(0, 1000)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_NOT_INSIDE, "pattern4"),
        RuleExpr(OPERATORS.AND_NOT, "pattern1"),
        RuleExpr(OPERATORS.AND, "pattern2"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([]), f"{result}"


def testE() -> None:
    """
    let pattern1 = bad(...)
    let pattern2 = def __eq__(): \n...
    let pattern3 = def normal(): \n...


        0-100 def __eq__():
        100-200    bad()

        200-300 def normal():
        300-400     bad(bad())
        400-500     def __eq__():
        500-600         bad()
    """

    """
        and-inside P3
        and-not-inside P2
        and P1
        OUTPUT: [300-400], [350-400]
    """
    results = {
        PatternId("pattern1"): [
            PatternMatchMock(100, 200),
            PatternMatchMock(300, 400),
            PatternMatchMock(350, 400),
            PatternMatchMock(500, 600),
        ],
        PatternId("pattern2"): [PatternMatchMock(0, 200), PatternMatchMock(400, 600)],
        PatternId("pattern3"): [PatternMatchMock(200, 600)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_INSIDE, "pattern3"),
        RuleExpr(OPERATORS.AND_NOT_INSIDE, "pattern2"),
        RuleExpr(OPERATORS.AND, "pattern1"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(300, 400, {}), Range(350, 400, {})]), f"{result}"

    """
        and-inside P2
        and-not-inside P3
        and P1
        OUTPUT: [100-200]
    """
    expression = [
        RuleExpr(OPERATORS.AND_INSIDE, "pattern2"),
        RuleExpr(OPERATORS.AND_NOT_INSIDE, "pattern3"),
        RuleExpr(OPERATORS.AND, "pattern1"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(100, 200, {})]), f"{result}"

    """
        and-inside P1
        OUTPUT: [100-200, 300-400, 350-400, 500-600]
    """
    expression = [RuleExpr(OPERATORS.AND_INSIDE, "pattern1")]
    result = evaluate_expression(expression, results)
    assert result == set(
        [
            Range(100, 200, {}),
            Range(300, 400, {}),
            Range(350, 400, {}),
            Range(500, 600, {}),
        ]
    ), f"{result}"

    """
        and-inside-noteq P1
        and P1
        OUTPUT: [350-400]
    """

    # expression = [(OPERATORS.AND_INSIDE_NOT_EQ, ["pattern1"]),
    #              (OPERATORS.AND, ["pattern1"])]
    # result = evaluate_expression(expression, results)
    # assert result == set([Range(350, 400)]), f"{result}"
    # TODO


def testF() -> None:
    """Nested boolean expressions

    let pattern1 = bad(..., x=1)
    let pattern2 = bad(..., y=2)
    let pattern3 = def normal(): \n...
    let pattern4 = def unusual(): \n...

    We want to find (ONLY inside P3), either:
        P2 inside P4
      or
        P1 not-inside P4

    example:

    000-100    def normal():
    100-200        bad(x = 1) # P1 not-inside-P4
    200-300        bad(y = 2) # no match
    300-400        def unusual():
    400-500            bad(x = 1) # no match
    500-600            bad(y = 2) # P2 inside P4
    600-700        def regular():
    700-800            bad(x = 1) # P1 not-inside P4
    800-900            bad(y = 2) # no-match

        pattern-inside P3
        and-either:
           - patterns:
            - pattern-inside P4
            - and P2
          - patterns:
            - pattern-not-inside P4
            - and P1

        OUTPUT: [500-600], [700-800]
    """
    results = {
        PatternId("pattern1"): [
            PatternMatchMock(100, 200),
            PatternMatchMock(400, 500),
            PatternMatchMock(700, 800),
        ],
        PatternId("pattern2"): [
            PatternMatchMock(200, 300),
            PatternMatchMock(500, 600),
            PatternMatchMock(800, 900),
        ],
        PatternId("pattern3"): [PatternMatchMock(0, 900)],
        PatternId("pattern4"): [PatternMatchMock(300, 600)],
    }

    subexpression1 = [
        RuleExpr(OPERATORS.AND_INSIDE, "pattern4"),
        RuleExpr(OPERATORS.AND, "pattern2"),
    ]
    subexpression2 = [
        RuleExpr(OPERATORS.AND_NOT_INSIDE, "pattern4"),
        RuleExpr(OPERATORS.AND, "pattern1"),
    ]
    expression = [
        RuleExpr(OPERATORS.AND_INSIDE, "pattern3"),
        BooleanRuleExpression(
            OPERATORS.AND_EITHER,
            None,
            [
                BooleanRuleExpression(OPERATORS.AND_ALL, None, subexpression1),
                BooleanRuleExpression(OPERATORS.AND_ALL, None, subexpression2),
            ],
        ),
    ]
    result = evaluate_expression(expression, results)
    assert result == set(
        [Range(100, 200, {}), Range(500, 600, {}), Range(700, 800, {})]
    ), f"{result}"

    # TODO test and-all (`patterns` subkey)


def test_exprs() -> None:
    subexpression1 = [
        BooleanRuleExpression(OPERATORS.AND_INSIDE, PatternId("pattern4"), None, "p4"),
        BooleanRuleExpression(OPERATORS.AND, PatternId("pattern2"), None, "p2"),
    ]
    subexpression2 = [
        BooleanRuleExpression(
            OPERATORS.AND_NOT_INSIDE, PatternId("pattern4"), None, "p4"
        ),
        BooleanRuleExpression(OPERATORS.AND, PatternId("pattern1"), None, "p1"),
    ]
    expression = BooleanRuleExpression(
        OPERATORS.AND_ALL,
        None,
        [
            BooleanRuleExpression(
                OPERATORS.AND_INSIDE, PatternId("pattern3"), None, "p3"
            ),
            BooleanRuleExpression(
                OPERATORS.AND_EITHER,
                None,
                [
                    BooleanRuleExpression(
                        OPERATORS.AND_ALL, PatternId("someid"), subexpression1
                    ),
                    BooleanRuleExpression(
                        OPERATORS.AND_ALL, PatternId("someid2"), subexpression2
                    ),
                ],
            ),
        ],
    )
    flat = list(enumerate_patterns_in_boolean_expression(expression))
    # print(flat)

    expected = [
        BooleanRuleExpression(OPERATORS.AND_ALL, None, None, None),
        BooleanRuleExpression(OPERATORS.AND_INSIDE, PatternId("pattern3"), None, "p3"),
        BooleanRuleExpression(OPERATORS.AND_EITHER, None, None, None),
        BooleanRuleExpression(OPERATORS.AND_ALL, None, None, None),
        BooleanRuleExpression(OPERATORS.AND_INSIDE, PatternId("pattern4"), None, "p4"),
        BooleanRuleExpression(OPERATORS.AND, PatternId("pattern2"), None, "p2"),
        BooleanRuleExpression(OPERATORS.AND_ALL, None, None, None),
        BooleanRuleExpression(
            OPERATORS.AND_NOT_INSIDE, PatternId("pattern4"), None, "p4"
        ),
        BooleanRuleExpression(OPERATORS.AND, PatternId("pattern1"), None, "p1"),
    ]

    assert flat == expected, f"flat: {flat}"


def test_build_exprs() -> None:
    base_rule: Dict[str, Any] = {
        "id": "test-id",
        "message": "test message",
        "languages": ["python"],
        "severity": "ERROR",
    }
    rules: List[Dict[str, Any]] = [
        {**base_rule, **{"pattern": "test(...)"}},
        {**base_rule, **{"patterns": [{"pattern": "test(...)"}]}},
        {**base_rule, **{"pattern-either": [{"pattern": "test(...)"}]}},
    ]

    results = [Rule.from_json(rule).expression for rule in rules]
    base_expected = [
        BooleanRuleExpression(OPERATORS.AND, PatternId(".0"), None, "test(...)")
    ]
    expected = [
        BooleanRuleExpression(OPERATORS.AND, PatternId("test-id"), None, "test(...)"),
        BooleanRuleExpression(OPERATORS.AND_ALL, None, base_expected, None),
        BooleanRuleExpression(OPERATORS.AND_EITHER, None, base_expected, None),
    ]

    assert results == expected


def test_evaluate_python() -> None:
    """Test evaluating the subpattern `where-python: <python_expression>`,
    in which a rule can provide an arbitrary Python expression that will be
    evaluated against the currently matched metavariables.

    NOTE: the Python expression must evaluate to True or False.

    NOTE: Assume patterns are applied in the order specified, top to bottom.

    This is implementing: https://github.com/returntocorp/semgrep/issues/101.

        let allExecs = exec($X)
        let filteredExecs = where-python: "vars['$X'].startswith('cmd')"

        000-100   var exec = require('child_process').exec;

        100-200   var cmd_pattern = "user_input";
        200-300   var other_pattern = "hardcoded_string";

        300-400   // should match
        400-500   exec(cmd_pattern, function(error, stdout, stderr){
        500-600       console.log(stdout);
        600-700   });

        700-800   // should not match
        800-900   exec(other_pattern, function(error, stdout, stderr){
        900-1000      console.log(stdout);
        1100-1200 });

        patterns:
            pattern: exec($X)
            where-python: "vars['$X'].startswith('cmd')"


        OUTPUT: [400-500]
    """
    results = {
        PatternId("all_execs"): [
            PatternMatchMock(400, 500, {"$X": {"abstract_content": "cmd_pattern"}}),
            PatternMatchMock(800, 900, {"$X": {"abstract_content": "other_pattern"}}),
        ]
    }

    expression = [
        BooleanRuleExpression(OPERATORS.AND, PatternId("all_execs"), None, "all_execs"),
        BooleanRuleExpression(
            OPERATORS.WHERE_PYTHON,
            PatternId("p1"),
            None,
            "vars['$X'].startswith('cmd')",
        ),
    ]

    result = evaluate_expression(expression, results, allow_exec=True)
    assert result == set([Range(400, 500, {})]), f"{result}"


def test_evaluate_python_exec_false() -> None:
    results = {
        PatternId("all_execs"): [
            PatternMatchMock(400, 500, {"$X": {"abstract_content": "cmd_pattern"}}),
            PatternMatchMock(800, 900, {"$X": {"abstract_content": "other_pattern"}}),
        ]
    }

    expression = [
        BooleanRuleExpression(OPERATORS.AND, PatternId("all_execs"), None, "all_execs"),
        BooleanRuleExpression(
            OPERATORS.WHERE_PYTHON,
            PatternId("p1"),
            None,
            "vars['$X'].startswith('cmd')",
        ),
    ]

    with pytest.raises(SemgrepError):
        evaluate_expression(expression, results, allow_exec=False)


def test_evaluate_python_bad_return_type() -> None:
    results = {
        PatternId("all_execs"): [
            PatternMatchMock(400, 500, {"$X": {"abstract_content": "cmd_pattern"}}),
            PatternMatchMock(800, 900, {"$X": {"abstract_content": "other_pattern"}}),
        ]
    }

    expression = [
        BooleanRuleExpression(OPERATORS.AND, PatternId("all_execs"), None, "all_execs"),
        BooleanRuleExpression(
            OPERATORS.WHERE_PYTHON,
            PatternId("p1"),
            None,
            "str(vars['$X'])",
        ),
    ]

    with pytest.raises(SemgrepError):
        evaluate_expression(expression, results, allow_exec=True)


def test_single_pattern_match_filtering() -> None:
    results = {
        PatternId("pattern1"): [PatternMatchMock(30, 100, {"$X": "x1", "$Y": "y1"})],
        PatternId("pattern2"): [PatternMatchMock(30, 100, {"$X": "x1", "$Y": "y2"})],
        PatternId("pattern3"): [PatternMatchMock(30, 100, {"$X": "x1"})],
    }
    expression = [
        BooleanRuleExpression(
            OPERATORS.AND_EITHER,
            None,
            [RuleExpr(OPERATORS.AND, "pattern1"), RuleExpr(OPERATORS.AND, "pattern2")],
        ),
        RuleExpr(OPERATORS.AND_NOT, "pattern3"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set(), f"{result}"
