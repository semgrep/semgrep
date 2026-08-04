# Regression tests for 64-bit integer overflow detection in constant folding.
# Each "ok" case wraps to one of the four testable pattern values if the
# corresponding overflow check is removed.
# (Cases that wrap to min_int cannot be tested: 2^63 overflows int64 parsing.)
# --- Positive: constants that should fold and match ---
# ruleid: const-fold-overflow
sink(9223372036854775807)

# ruleid: const-fold-overflow
sink(3 - 3)

# --- Sub overflow: min_int - 1 and min_int + (-1) wrap to max_int ---
# ok: const-fold-overflow
sink((-9223372036854775807 - 1) - 1)
# ok: const-fold-overflow
sink((-9223372036854775807 - 1) + (-1))

# --- Mul/shift overflow: max_int * 2 and max_int << 1 wrap to -2 ---
# ok: const-fold-overflow
sink(9223372036854775807 * 2)
# ok: const-fold-overflow
sink(2 * 9223372036854775807)
# ok: const-fold-overflow
sink(9223372036854775807 << 1)

# --- Mul/shift overflow: min_int * k and min_int << 1 wrap to 0 ---
# Without the explicit (i1 = min_int && i2 >= 2) guard, abs(min_int) wraps
# in int64 and defeats the abs-based overflow check.
# ok: const-fold-overflow
sink((-9223372036854775807 - 1) * 2)
# ok: const-fold-overflow
sink(2 * (-9223372036854775807 - 1))
# ok: const-fold-overflow
sink((-9223372036854775807 - 1) * 4)
# ok: const-fold-overflow
sink(4 * (-9223372036854775807 - 1))
# ok: const-fold-overflow
sink((-9223372036854775807 - 1) << 1)


# --- Intraprocedural (IL) overflow detection: Eval_il_partial ---
# Same checks, but exercised through the flow-sensitive IL svalue analysis: the
# reassigned local forces the IL pass (not the generic pass) to do the folding.
def _il_overflow():
    # Folds normally; confirms the rule fires through the IL pass.
    a = 0
    a = 4 - (-9223372036854775803)  # 9223372036854775807 = max_int
    # ruleid: const-fold-overflow
    sink(a)
    # Subtraction overflow: min_int - 1 wraps to max_int if the check is removed.
    b = 0
    b = (-9223372036854775807 - 1) - 1
    # ok: const-fold-overflow
    sink(b)
    # Multiplication overflow: max_int * 2 wraps to -2 if the check is removed.
    c = 0
    c = 9223372036854775807 * 2
    # ok: const-fold-overflow
    sink(c)
    # Shift overflow: max_int << 1 wraps to -2 if the check is removed.
    d = 0
    d = 9223372036854775807 << 1
    # ok: const-fold-overflow
    sink(d)
