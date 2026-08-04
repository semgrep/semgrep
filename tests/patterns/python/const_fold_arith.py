a = 2 + 4  # addition
b = 10 - 4  # subtraction
c = 2 * 3  # multiplication

d = 18 // 3  # floor division
d1 = 20 // 3  # floor division

d2 = 18 / 3  # true division
d3 = 18 / 4  # true division with non-integer result
dzero = 18 / 0 


d4 = -22 // 4
d5 = 20 // 0

e = 27 % 7  # modulo (27 = 3*7 + 6)
f = 2**2 + 2  # exponentiation
g = -(4 - 10)  # unary minus
h = +(3 + 3)  # unary plus (identity)

# ERROR:
sink(a)
# ERROR:
sink(b)
# ERROR:
sink(c)

# ERROR:
sink(d)
# ERROR:
sink(d1)
# ERROR:
sink(d2)
# OK:
sink(d3)
# ERROR:
sink(-d4)
# OK:
sink(d5)
# OK: 
sink(dzero)

# ERROR:
sink(e)
# ERROR:
sink(f)
# ERROR:
sink(g)
# ERROR:
sink(h)


# --- Intraprocedural (IL) constant folding: Eval_il_partial ---
# The module-level cases above are folded by the syntactic (generic) pass.
# Reassigning a local defeats that pass (it only propagates single-assignment
# vars), so the cases below are folded by the flow-sensitive IL svalue
# analysis used inside function bodies. Each still folds to 6.
def _il_arith():
    add = 0
    add = 4 + 2
    # ERROR:
    sink(add)
    sub = 0
    sub = 10 - 4
    # ERROR:
    sink(sub)
    mul = 0
    mul = 2 * 3
    # ERROR:
    sink(mul)
    div = 0
    div = 18 / 3  # exact division
    # ERROR:
    sink(div)
    fdiv = 0
    fdiv = 20 // 3  # floor division
    # ERROR:
    sink(fdiv)
    mod = 0
    mod = 27 % 7
    # ERROR:
    sink(mod)
    powr = 0
    powr = 2**2 + 2  # exponentiation
    # ERROR:
    sink(powr)
    um = 0
    um = -(4 - 10)  # unary minus
    # ERROR:
    sink(um)
    up = 0
    up = +(3 + 3)  # unary plus (identity)
    # ERROR:
    sink(up)
    # Mixed-sign floor division rounds toward -inf: -22 // 4 = -6.
    # Negating gives a positive, matchable value (6).
    fdneg = 0
    fdneg = -22 // 4
    # ERROR:
    sink(-fdneg)
