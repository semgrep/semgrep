# Tests for SCCP (Sparse Conditional Constant Propagation).
#
# SCCP: when a branch condition evaluates to a known literal, the dead branch
# contributes nothing to the join point, allowing constants to survive even
# when only one branch assigns them.
#
# The Dead/Live/Unprocessed lattice also ensures that constants are propagated
# INTO dead branches (so findings inside dead code still fire), but assignments
# inside the dead branch do not escape to the join.
# --- Basic dead-branch elimination ---


def test_dead_true_branch():
    # x=0, so `if x == 1` is always false; only the else branch is live.
    x = 0
    if x == 1:
        y = "dead"
    else:
        y = "live"
    # proruleid: test-sccp
    foo(y)


def test_dead_false_branch():
    # x=1, so the else branch is always dead.
    x = 1
    if x == 1:
        y = "live"
    else:
        y = "dead"
    # proruleid: test-sccp
    foo(y)


def test_unknown_condition(c):
    # Condition is not statically known: both branches are live.
    # Must-analysis: y is Cst(Cstr) at join — we know it's a string but
    # not which one. The pattern foo("live") requires an exact literal match,
    # so this does NOT match.
    if c:
        y = "yes"
    else:
        y = "no"
    # ok:
    foo(y)


# --- Arithmetic and bitwise conditions ---


def test_arithmetic_condition():
    # 3 + 4 = 7, so `n == 7` is always true.
    n = 3 + 4
    if n == 7:
        y = "live"
    else:
        y = "dead"
    # proruleid: test-sccp
    foo(y)


def test_comparison_chain():
    # 2 * 6 = 12, 12 - 4 = 8, 8 > 5 is true.
    x = 2 * 6 - 4
    if x > 5:
        y = "live"
    else:
        y = "dead"
    # proruleid: test-sccp
    foo(y)


def test_bitwise_condition():
    # 5 & 6 = 4  (101 & 110 = 100), so `== 4` is always true.
    x = 5
    y = 6
    if (x & y) == 4:
        z = "live"
    else:
        z = "dead"
    # proruleid: test-sccp
    foo(z)


# --- Only defined in live branch ---


def test_only_defined_in_live_branch():
    # z is only assigned in the live branch.
    # SCCP: FalseNode(x==0) is dead (x==0 is always true), contributes nothing
    # at the join, so z="live" survives.
    x = 0
    if x == 0:
        z = "live"
    # proruleid: test-sccp
    foo(z)


# --- Propagation into dead branches ---


def test_dead_branch_propagation():
    # Constants flow INTO dead branches (for matching), but assignments
    # inside the dead branch do not escape to the join.
    y = "live"
    x = 0
    if x == 1:
        # In CE this folds to foo("live"); in Pro the branch is dead and pruned.
        # ruleid: prook: test-sccp
        foo(y)
        y = "dead"  # this assignment stays inside the dead branch
    # proruleid: test-sccp
    foo(y)


# --- Nested conditions ---


def test_nested_dead_outer():
    # Outer condition is false (x=0, x==1 fails); inner branch never runs.
    x = 0
    if x == 1:
        if bar():
            y = "dead"
        else:
            y = "dead"
    else:
        y = "live"
    # proruleid: test-sccp
    foo(y)


def test_nested_dead_inner():
    # Outer is live (x=1), inner else is dead (y=0, y==1 fails).
    x = 1
    if x == 1:
        y = 0
        if y == 1:
            z = "dead"
        else:
            z = "live"
    else:
        z = "dead"
    # proruleid: test-sccp
    foo(z)


# --- Comparison-operator conditions ---


def test_not_equal_condition():
    # x=1, so `x != 1` is false — the if-branch is dead.
    x = 1
    if x != 1:
        y = "dead"
    else:
        y = "live"
    # proruleid: test-sccp
    foo(y)


def test_less_than_condition():
    # x=3, so `x < 5` is true — the else-branch is dead.
    x = 3
    if x < 5:
        y = "live"
    else:
        y = "dead"
    # proruleid: test-sccp
    foo(y)


def test_greater_than_or_equal_condition():
    # x=5, so `x >= 5` is true — the else-branch is dead.
    x = 5
    if x >= 5:
        y = "live"
    else:
        y = "dead"
    # proruleid: test-sccp
    foo(y)


def test_less_than_or_equal_false():
    # x=10, so `x <= 5` is false — the if-branch is dead.
    x = 10
    if x <= 5:
        y = "dead"
    else:
        y = "live"
    # proruleid: test-sccp
    foo(y)


# --- elif chain ---


def test_elif_dead_first_branch():
    # x=2: first elif (`x == 1`) is dead, second (`x == 2`) is live.
    x = 2
    if x == 1:
        y = "dead"
    elif x == 2:
        y = "live"
    else:
        y = "dead"
    # proruleid: test-sccp
    foo(y)


# --- Loop-invariant constant then SCCP ---


def test_loop_invariant_drives_sccp():
    # x is constant before the loop and never written inside it.
    # After the loop, the condition `x == 1` is still evaluable by SCCP.
    x = 1
    for i in range(5):
        pass  # x untouched
    if x == 1:
        y = "live"
    else:
        y = "dead"
    # proruleid: test-sccp
    foo(y)
