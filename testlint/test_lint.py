import os
import sys

from sgrep import OPERATORS, Range, evaluate_expression

# run from parent directory with PYTHONPATH=. python3 testlint/test_lint.py


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
        "pattern1": [Range(30, 100)],
        "pattern2": [Range(0, 100), Range(30, 100)],
    }
    expression = [(OPERATORS.AND_NOT, ["pattern1"]), (OPERATORS.AND, ["pattern2"])]
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
        "pattern1": [Range(0, 100)],
        "pattern2": [Range(30, 70), Range(0, 100)],
        "pattern3": [],
    }
    expression = [
        (OPERATORS.AND_NOT, ["pattern1"]),
        (OPERATORS.AND_EITHER, ["pattern2", "pattern3"]),
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
        "pattern1": [Range(100, 1000)],
        "pattern2": [Range(100, 1000), Range(200, 300)],
        "pattern4": [Range(0, 1000)],
    }
    expression = [
        (OPERATORS.AND_INSIDE, ["pattern4"]),
        (OPERATORS.AND_NOT, ["pattern1"]),
        (OPERATORS.AND, ["pattern2"]),
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
        "pattern1": [Range(100, 1000)],
        "pattern2": [Range(100, 1000), Range(200, 300)],
        "pattern4": [Range(0, 1000)],
    }
    expression = [
        (OPERATORS.AND_NOT_INSIDE, ["pattern4"]),
        (OPERATORS.AND_NOT, ["pattern1"]),
        (OPERATORS.AND, ["pattern2"]),
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
            Range(100, 200),
            Range(300, 400),
            Range(350, 400),
            Range(500, 600),
        ],
        "pattern2": [Range(0, 200), Range(400, 600)],
        "pattern3": [Range(200, 600)],
    }
    expression = [
        (OPERATORS.AND_INSIDE, ["pattern3"]),
        (OPERATORS.AND_NOT_INSIDE, ["pattern2"]),
        (OPERATORS.AND, ["pattern1"]),
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
        (OPERATORS.AND_INSIDE, ["pattern2"]),
        (OPERATORS.AND_NOT_INSIDE, ["pattern3"]),
        (OPERATORS.AND, ["pattern1"]),
    ]
    result = evaluate_expression(expression, results)
    assert result == set([Range(100, 200)]), f"{result}"

    """
        and-inside P1
        OUTPUT: [100-200, 300-400, 350-400, 500-600]
    """
    expression = [(OPERATORS.AND_INSIDE, ["pattern1"])]
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


def testAll():
    testA()
    testB()
    testC()
    testD()
    testE()


if __name__ == "__main__":
    testAll()
    print("all tests passed")
