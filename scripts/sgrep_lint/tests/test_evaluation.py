#!/usr/bin/env python3
import os
import sys
from typing import List

from evaluation import enumerate_patterns_in_boolean_expression
from evaluation import evaluate_expression
from sgrep_types import BooleanRuleExpression
from sgrep_types import Operator
from sgrep_types import OPERATORS
from sgrep_types import Range
from sgrep_types import SgrepRange


def SRange(start: int, end: int):
    return SgrepRange(Range(start, end), {})


def RuleExpr(
    operator: Operator,
    fake_pattern_name: str,
    children: List[BooleanRuleExpression] = None,
):
    return BooleanRuleExpression(
        operator, fake_pattern_name, children, "fake-pattern-text-here"
    )


def testA():
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
        "pattern1": [SRange(30, 100)],
        "pattern2": [SRange(0, 100), SRange(30, 100)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_NOT, "pattern1"),
        RuleExpr(OPERATORS.AND, "pattern2"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(0, 100)]), f"{result}"


def testB():
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
    results = {
        "pattern1": [SRange(0, 100)],
        "pattern2": [SRange(30, 70), SRange(0, 100)],
        "pattern3": [],
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
    assert result == set([Range(30, 70)]), f"{result}"


def testC():
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
        "pattern1": [SRange(100, 1000)],
        "pattern2": [SRange(100, 1000), SRange(200, 300)],
        "pattern4": [SRange(0, 1000)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_INSIDE, "pattern4"),
        RuleExpr(OPERATORS.AND_NOT, "pattern1"),
        RuleExpr(OPERATORS.AND, "pattern2"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(200, 300)]), f"{result}"


def testD():
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
        "pattern1": [SRange(100, 1000)],
        "pattern2": [SRange(100, 1000), SRange(200, 300)],
        "pattern4": [SRange(0, 1000)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_NOT_INSIDE, "pattern4"),
        RuleExpr(OPERATORS.AND_NOT, "pattern1"),
        RuleExpr(OPERATORS.AND, "pattern2"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([]), f"{result}"


def testE():
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
        "pattern1": [
            SRange(100, 200),
            SRange(300, 400),
            SRange(350, 400),
            SRange(500, 600),
        ],
        "pattern2": [SRange(0, 200), SRange(400, 600)],
        "pattern3": [SRange(200, 600)],
    }
    expression = [
        RuleExpr(OPERATORS.AND_INSIDE, "pattern3"),
        RuleExpr(OPERATORS.AND_NOT_INSIDE, "pattern2"),
        RuleExpr(OPERATORS.AND, "pattern1"),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(300, 400), Range(350, 400)]), f"{result}"

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
    assert result == set([Range(100, 200)]), f"{result}"

    """
        and-inside P1
        OUTPUT: [100-200, 300-400, 350-400, 500-600]
    """
    expression = [RuleExpr(OPERATORS.AND_INSIDE, "pattern1")]
    result = evaluate_expression(expression, results)
    assert result == set(
        [Range(100, 200), Range(300, 400), Range(350, 400), Range(500, 600)]
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


def testF():
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
        "pattern1": [SRange(100, 200), SRange(400, 500), SRange(700, 800)],
        "pattern2": [SRange(200, 300), SRange(500, 600), SRange(800, 900)],
        "pattern3": [SRange(0, 900)],
        "pattern4": [SRange(300, 600)],
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
        [Range(100, 200), Range(500, 600), Range(700, 800)]
    ), f"{result}"

    # TODO test and-all (`patterns` subkey)


def test_exprs():
    subexpression1 = [
        BooleanRuleExpression(OPERATORS.AND_INSIDE, "pattern4", None, "p4"),
        BooleanRuleExpression(OPERATORS.AND, "pattern2", None, "p2"),
    ]
    subexpression2 = [
        BooleanRuleExpression(OPERATORS.AND_NOT_INSIDE, "pattern4", None, "p4"),
        BooleanRuleExpression(OPERATORS.AND, "pattern1", None, "p1"),
    ]
    expression = [
        BooleanRuleExpression(OPERATORS.AND_INSIDE, "pattern3", None, "p3"),
        BooleanRuleExpression(
            OPERATORS.AND_EITHER,
            None,
            [
                BooleanRuleExpression(OPERATORS.AND_ALL, "someid", subexpression1),
                BooleanRuleExpression(OPERATORS.AND_ALL, "someid2", subexpression2),
            ],
        ),
    ]
    flat = list(enumerate_patterns_in_boolean_expression(expression))
    # print(flat)

    expected = [
        BooleanRuleExpression(OPERATORS.AND_INSIDE, "pattern3", None, "p3"),
        BooleanRuleExpression(OPERATORS.AND_EITHER, None, None, None),
        BooleanRuleExpression(OPERATORS.AND_ALL, None, None, None),
        BooleanRuleExpression(OPERATORS.AND_INSIDE, "pattern4", None, "p4"),
        BooleanRuleExpression(OPERATORS.AND, "pattern2", None, "p2"),
        BooleanRuleExpression(OPERATORS.AND_ALL, None, None, None),
        BooleanRuleExpression(OPERATORS.AND_NOT_INSIDE, "pattern4", None, "p4"),
        BooleanRuleExpression(OPERATORS.AND, "pattern1", None, "p1"),
    ]

    assert flat == expected, f"flat: {flat}"


def testEvaluatePython():
    """Test evaluating the subpattern `where-python: <python_expression>`,
    in which a rule can provide an arbitrary Python expression that will be
    evaluated against the currently matched metavariables.

    NOTE: the Python expression must evaluate to True or False.

    NOTE: Assume patterns are applied in the order specified, top to bottom.

    This is implementing: https://github.com/returntocorp/sgrep/issues/101.

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
        "all_execs": [
            SgrepRange(Range(400, 500), {"$X": "cmd_pattern"}),
            SgrepRange(Range(800, 900), {"$X": "other_pattern"}),
        ]
    }

    expression = [
        BooleanRuleExpression(OPERATORS.AND, "all_execs", None, "all_execs"),
        BooleanRuleExpression(
            OPERATORS.WHERE_PYTHON, "p1", None, "vars['$X'].startswith('cmd')"
        ),
    ]

    result = evaluate_expression(expression, results)
    assert result == set([Range(400, 500)]), f"{result}"
