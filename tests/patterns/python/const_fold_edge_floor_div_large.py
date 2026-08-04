# Floor division with dividends >= 2^62 must fold to the correct result.
#
# The old sign() function shifted by (Sys.int_size - 1) = 62 instead of 63.
# This caused any positive Int64 >= 2^62 to be misidentified as "negative",
# triggering the floor-div "different signs" adjustment even when both
# operands were positive.
#
# Concrete failure: 9223372036854775807 // 2
#   Correct:  9223372036854775807 / 2 = 4611686018427387903  (exact floor)
#   Old bug:  sign(max_int) = 1 (wrong), sign(2) = 0, treated as "different
#             signs", remainder=1≠0 → computed 4611686018427387903 - 1 = 4611686018427387902.
#
# The correct result 4611686018427387903 matches the pattern; the buggy
# result does not, so this was a false negative before the fix.
# ERROR:
sink(9223372036854775807 // 2)

# Exact floor division with large dividend should also fold correctly.
# 9223372036854775806 // 2 = 4611686018427387903 (no remainder, same result).
# ERROR:
sink(9223372036854775806 // 2)

# Negative dividend / positive divisor — a genuinely different-signs case
# that must still fold correctly (floor rounds toward -infinity).
# -9223372036854775807 // 2 = -4611686018427387904  (not equal to pattern, so no match)
sink(-9223372036854775807 // 2)


# --- Intraprocedural (IL) constant folding: Eval_il_partial ---
# Same large-dividend sign_bit regression, but exercised through the IL svalue
# analysis (reassigned local inside a function body) rather than the generic
# pass. Must still fold to the exact floor.
def _il_floor_div_large():
    x = 0
    x = 9223372036854775807 // 2
    # ERROR:
    sink(x)
