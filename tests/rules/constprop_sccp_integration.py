# Integration tests combining SCCP with bitwise/arithmetic evaluation.
#
# These tests exercise multi-hop constant propagation where:
#   1. Arithmetic or bitwise ops produce a literal value
#   2. That value drives a branch condition (evaluated by SCCP)
#   3. A string constant survives the dead-branch elimination to a sink

# --- Bitwise result drives SCCP condition ---

def test_bitwise_drives_sccp():
    # 5 & 6 = 4; condition `== 4` is true, else branch is dead.
    flags = 5
    mask = 6
    if (flags & mask) == 4:
        result = "yes"
    else:
        result = "no"   # dead
    #proruleid: test
    sink(result)

def test_xor_drives_sccp():
    # 0b1010 ^ 0b1100 = 0b0110 = 6; `!= 6` is false, else branch is live.
    a = 0b1010   # 10
    b = 0b1100   # 12
    if (a ^ b) != 6:
        result = "no"   # dead (condition is false)
    else:
        result = "yes"
    #proruleid: test
    sink(result)

def test_shift_drives_sccp():
    # 7 << 4 = 112; `== 112` is true.
    key = 7
    shifted = key << 4
    if shifted == 112:
        status = "yes"
    else:
        status = "no"   # dead
    #proruleid: test
    sink(status)

# --- Multi-hop: arithmetic chain then SCCP ---

def test_multi_hop_arithmetic():
    # Each step produces a literal; the final comparison drives SCCP.
    a = 10
    b = a * 3        # 30
    c = b - 6        # 24
    d = c // 2       # 12
    if d == 12:
        answer = "yes"
    else:
        answer = "no"   # dead
    #proruleid: test
    sink(answer)

def test_multi_hop_bitwise():
    # XOR chain produces the original secret; comparison drives SCCP.
    pad = 204
    secret = 112
    public = pad ^ secret   # 188
    recovered = public ^ pad  # 112
    if recovered == 112:
        verdict = "yes"
    else:
        verdict = "no"   # dead
    #proruleid: test
    sink(verdict)

# --- SCCP does not fire on unknown inputs ---

def test_unknown_operand(user_input):
    # user_input is not constant; bitwise result is Cst not Lit.
    flags = user_input & 7
    if flags == 4:
        result = "yes"
    else:
        result = "no"
    # OK: condition is not statically evaluable
    sink(result)

def test_partially_known(c):
    # Even though one operand is known, the other isn't.
    mask = 6
    if (c & mask) == 4:
        result = "yes"
    else:
        result = "no"
    # OK: c is not constant
    sink(result)

# --- SCCP with while-loop invariant ---

def test_loop_invariant_then_sccp():
    # x is constant before the loop and not modified inside it;
    # the post-loop condition can be evaluated by SCCP.
    x = 42
    total = 0
    for _ in range(3):
        total += 1   # total is not constant after loop
    if x == 42:
        label = "yes"
    else:
        label = "no"   # dead: x never changes
    #proruleid: test
    sink(label)

# --- Nested SCCP ---

def test_nested_sccp():
    # Both outer and inner conditions are evaluable.
    # Outer: 2+2==4 true; inner: 10//3==3 true.
    outer = 2 + 2
    if outer == 4:
        inner = 10 // 3
        if inner == 3:
            result = "yes"
        else:
            result = "no"   # dead
    else:
        result = "no"       # dead
    #proruleid: test
    sink(result)

# --- New operators driving SCCP ---

def test_floordiv_neg_drives_sccp():
    # -7 // 2 = -4; condition `< 0` is true (avoids matching on negative literal).
    x = -7
    d = x // 2
    if d < 0:
        result = "yes"
    else:
        result = "no"   # dead
    #proruleid: test
    sink(result)

def test_mod_drives_sccp():
    # 7 % 3 = 1; condition `== 1` is true.
    a = 7
    b = a % 3
    if b == 1:
        result = "yes"
    else:
        result = "no"   # dead
    #proruleid: test
    sink(result)

def test_pow_drives_sccp():
    # 2 ** 10 = 1024; condition `== 1024` is true.
    p = 2 ** 10
    if p == 1024:
        result = "yes"
    else:
        result = "no"   # dead
    #proruleid: test
    sink(result)

def test_bitnot_drives_sccp():
    # ~(-113) = 112; condition `== 112` is true.
    x = ~(-113)
    if x == 112:
        result = "yes"
    else:
        result = "no"   # dead
    #proruleid: test
    sink(result)

def test_not_equal_drives_sccp():
    # 5 != 6 is True; the else-branch (result="no") is dead.
    a = 5
    b = 6
    if a != b:
        result = "yes"
    else:
        result = "no"   # dead
    #proruleid: test
    sink(result)

def test_comparison_lte_drives_sccp():
    # 3 <= 5 is True.
    a = 3
    if a <= 5:
        result = "yes"
    else:
        result = "no"   # dead
    #proruleid: test
    sink(result)

# --- New operators do NOT fire on unknown inputs ---

def test_floordiv_unknown_does_not_drive_sccp(n):
    # n is not constant; floor division result is Cst not Lit.
    d = n // 2
    if d == -4:
        result = "yes"
    else:
        result = "no"
    # OK: condition is not statically evaluable
    sink(result)

def test_mod_unknown_does_not_drive_sccp(n):
    b = n % 3
    if b == 1:
        result = "yes"
    else:
        result = "no"
    # OK:
    sink(result)
