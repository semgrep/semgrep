# Tests for bitwise, shift, unary, floor-division, modulo, and power operator
# constant folding in eval_binop_int / eval_unop_int.
#
# Rules (see constprop_bitwise.yaml):
#   sink(112)   — exact integer 112
#   sink(-4)    — negative floor-division result
#   sink(1024)  — power result

# --- XOR ---

def test_xor_basic():
    # XOR of two literals: 204 ^ 112 = 188 (not 112).
    x = 204
    y = 112
    z = x ^ y
    #ok: test
    sink(z)  # z = 188

def test_xor_chain():
    # XOR is its own inverse: (pad ^ secret) ^ pad = secret.
    pad = 204
    secret = 112
    public = pad ^ secret   # 188
    recovered = public ^ pad  # 112
    #ruleid: test
    sink(recovered)

def test_xor_self():
    # x ^ x = 0, not 112.
    x = 112
    z = x ^ x
    #ok: test
    sink(z)

# --- AND ---

def test_bitwise_and():
    # 255 & 112 = 112  (11111111 & 01110000 = 01110000)
    x = 255
    y = 112
    z = x & y
    #ruleid: test
    sink(z)

def test_bitwise_and_no_match():
    # 204 & 112 = 104  (11001100 & 01110000 = 01000000 = 64)
    # Wait: 11001100 & 01110000 = 01000000 = 64 ≠ 112
    x = 204
    y = 112
    z = x & y
    #ok: test
    sink(z)

# --- OR ---

def test_bitwise_or():
    # 96 | 16 = 112  (01100000 | 00010000 = 01110000)
    x = 96
    y = 16
    z = x | y
    #ruleid: test
    sink(z)

# --- Shifts ---

def test_left_shift():
    # 7 << 4 = 112  (0b111 << 4 = 0b1110000)
    x = 7
    z = x << 4
    #ruleid: test
    sink(z)

def test_right_shift():
    # 224 >> 1 = 112  (0b11100000 >> 1 = 0b01110000)
    x = 224
    z = x >> 1
    #ruleid: test
    sink(z)

def test_arith_right_shift_negative():
    # Python >> is ASR (arithmetic, sign-extending): -224 >> 1 = -112.
    # Result is negative, so tested via SCCP to keep the sink pattern positive.
    x = -224
    z = x >> 1  # -112
    if z < 0:
        result = "yes"
    else:
        result = "no"  # dead
    #proruleid: test-sccp
    sink(result)

# --- Non-constant input ---

def test_non_constant_operand():
    # XOR with non-constant: result is not a literal.
    pad = 204
    z = pad ^ get_secret()
    #ok: test
    sink(z)

# --- BitNot (~) ---

def test_bitnot_to_112():
    # ~(-113) = 112  (bitwise NOT of -113 in two's complement)
    x = -113
    z = ~x
    #ruleid: test
    sink(z)

def test_bitnot_double_is_identity():
    # ~~112 = 112
    x = 112
    z = ~~x
    #ruleid: test
    sink(z)

def test_bitnot_positive_is_negative():
    # ~42 = -43 (not 112)
    x = 42
    z = ~x
    #ok: test
    sink(z)

# --- Floor division ---
# Note: semgrep does not reliably match negative integer literals in patterns,
# so mixed-sign cases (result is negative) are tested via SCCP below.

def test_floordiv_positive():
    # 224 // 2 = 112
    x = 224
    z = x // 2
    #ruleid: test
    sink(z)

def test_floordiv_both_negative():
    # Both operands negative → floor = truncation; -224 // -2 = 112
    x = -224
    z = x // -2
    #ruleid: test
    sink(z)

def test_floordiv_chain():
    # Multi-hop: 10*3=30; 30-6=24; 24//2=12; 12*9+4=112
    a = 10 * 3   # 30
    b = a - 6    # 24
    c = b // 2   # 12
    d = c * 9    # 108
    e = d + 4    # 112
    #ruleid: test
    sink(e)

def test_floordiv_non_constant(n):
    # Divisor is unknown: cannot fold.
    z = n // 2
    #ok: test
    sink(z)

# Mixed-sign floor div produces a negative result; use SCCP so that the
# sink pattern stays positive (a string).

def test_floordiv_neg_dividend_drives_sccp():
    # -7 // 2 = -4  (floor toward -inf); -4 < 0 is always true.
    x = -7
    z = x // 2
    if z < 0:
        result = "yes"
    else:
        result = "no"  # dead
    #proruleid: test-sccp
    sink(result)

def test_floordiv_neg_divisor_drives_sccp():
    # 7 // -2 = -4  (floor toward -inf); -4 != 0 is true.
    x = 7
    z = x // -2
    if z != 0:
        result = "yes"
    else:
        result = "no"  # dead
    #proruleid: test-sccp
    sink(result)

# --- Modulo ---

def test_mod_to_112():
    # 312 % 200 = 112
    x = 312
    z = x % 200
    #ruleid: test
    sink(z)

def test_mod_zero_remainder():
    # 224 % 112 = 0 (not 112)
    x = 224
    z = x % 112
    #ok: test
    sink(z)

def test_mod_non_constant(n):
    # Modulus of unknown: result is not a literal.
    z = n % 200
    #ok: test
    sink(z)

# --- Power ---

def test_pow_to_1024():
    # 2 ** 10 = 1024
    x = 2
    z = x ** 10
    #ruleid: test-pow
    sink(z)

def test_pow_base_32():
    # 32 ** 2 = 1024
    x = 32
    z = x ** 2
    #ruleid: test-pow
    sink(z)

def test_pow_zero_exponent():
    # anything ** 0 = 1 (not 1024)
    x = 1024
    z = x ** 0
    #ok: test-pow
    sink(z)

def test_pow_non_constant(n):
    # Exponent is unknown: result is not a literal.
    z = 2 ** n
    #ok: test-pow
    sink(z)
