# Integer comparisons that produce True
a = (3 == 3)
b = (3 != 4)
c = (2 < 3)
d = (4 > 3)
e = (3 <= 3)
f = (3 >= 3)

# Boolean comparisons that produce True
g = (True == True)
h = (True != False)

# ERROR:
sink(a)
# ERROR:
sink(b)
# ERROR:
sink(c)
# ERROR:
sink(d)
# ERROR:
sink(e)
# ERROR:
sink(f)
# ERROR:
sink(g)
# ERROR:
sink(h)

# These fold to False and must not match sink(True)
i = (3 == 4)
j = (4 < 3)
sink(i)
sink(j)


# --- Intraprocedural (IL) constant folding: Eval_il_partial ---
# Reassigning each local forces the flow-sensitive IL svalue analysis (rather
# than the syntactic generic pass) to fold the comparison. True cases match.
def _il_cmp():
    a = None
    a = (3 == 3)
    # ERROR:
    sink(a)
    b = None
    b = (3 != 4)
    # ERROR:
    sink(b)
    c = None
    c = (2 < 3)
    # ERROR:
    sink(c)
    d = None
    d = (4 > 3)
    # ERROR:
    sink(d)
    e = None
    e = (3 <= 3)
    # ERROR:
    sink(e)
    f = None
    f = (3 >= 3)
    # ERROR:
    sink(f)
    # NOTE: boolean comparisons (True == True) are only folded by the generic
    # pass (see module level above); the IL evaluator folds integer comparisons
    # only, so they are not repeated here.
    # Fold to False: must not match sink(True).
    i = None
    i = (3 == 4)
    sink(i)
    j = None
    j = (4 < 3)
    sink(j)
