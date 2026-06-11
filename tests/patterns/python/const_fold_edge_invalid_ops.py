# Edge cases for operations that must NOT fold, grouped by failure reason.
# Pattern: sink(6).  The positive fold below confirms the rule fires at all.
#
# The value 6 is intentional: "6 << 64" and "6 << 128" would silently fold
# to 6 (via OCaml's (n & 63) mod on x86-64) without the valid_shift guard.

# Valid: 3 << 1 = 6.
x = 3 << 1
# ERROR:
sink(x)

# --- Shifts out of range (>= 64) ---
# Without a range check, OCaml's lsl/asr uses (amount & 63), so e.g.
#   6 << 64  -> 6 << 0 = 6  (would be a false positive here)
sink(6 << 64)
sink(6 << 128)
sink(1 << 63)
sink(1 >> 64)
sink(1 >> 63)

# --- Negative shift amounts ---
# Without a range check, (-1 & 63) = 63 on x86-64, so e.g.
#   0 << -1  -> 0 << 63 = 0  (false positive for sink(0), not sink(6))
sink(1 << -1)
sink(1 >> -1)
sink(6 << -1)

# --- Division / modulo by zero ---
sink(12 // 0)
sink(1 // 0)
sink(1 % 0)
sink(0 // 0)
sink(0 % 0)

# --- Negative integer exponent (result is a float in Python, not an int) ---
sink(2 ** -1)
sink((-3) ** -2)

# --- Mixed-sign modulo (Python floor-mod vs C/OCaml truncated-mod disagree) ---
# Python: (-7) % 2 = 1; C-style: -7 rem 2 = -1.
sink((-7) % 2)
# Python: 7 % (-3) = -2; C-style: 7 rem (-3) = 1.
sink(7 % (-3))
# Python: (-1) % 100 = 99; C-style: -1 rem 100 = -1.
sink((-1) % 100)


# --- Intraprocedural (IL) constant folding: Eval_il_partial ---
# The same guards must hold for the flow-sensitive IL svalue analysis. Each
# local is reassigned so the IL pass (not the generic pass) does the folding.
# Only the valid baseline folds to 6; every other case must NOT fold to 6.
def _il_invalid():
    # Valid baseline: 3 << 1 = 6, confirms the rule fires through the IL pass.
    x = 0
    x = 3 << 1
    # ERROR:
    sink(x)
    # Shift amount out of range (>= 64) must not fold (6 << 64 would wrap to 6
    # via OCaml's (n & 63) without the valid_shift guard).
    a = 0
    a = 6 << 64
    sink(a)
    # Negative shift amount must not fold.
    b = 0
    b = 6 << -1
    sink(b)
    # Division / modulo by zero must not fold (and must not crash).
    c = 0
    c = 12 // 0
    sink(c)
    d = 0
    d = 6 % 0
    sink(d)
    # Mixed-sign modulo must not fold: OCaml's 78 mod -12 = 6, but Python's
    # 78 % -12 = -6, so folding to 6 here would be unsound.
    e = 0
    e = 78 % (-12)
    sink(e)
    # Negative integer exponent (float result in Python) must not fold.
    f = 0
    f = 2 ** -1
    sink(f)
