# Pins the flow-sensitive svalue lattice at CFG join points, which is exactly
# what the `svalue option -> Unknown` migration reworked (union/refine + the
# `Unknown` bottom element in Eval_il_partial). Unlike constprop_dataflow.py,
# these use a *specific-value* pattern (`sink(42)`) so they distinguish a known
# literal (`Lit 42`) from a mere constant type (`Cst Cint`) at the join.

def same_branches(c):
    if c:
        x = 42
    else:
        x = 42
    # same literal on both paths -> union (Lit 42) (Lit 42) = Lit 42
    # ERROR:
    sink(x)

def diff_branches(c):
    if c:
        x = 42
    else:
        x = 99
    # different literals -> union (Lit 42) (Lit 99) = Cst Cint, not a known value
    # OK:
    sink(x)

def one_path(c):
    if c:
        x = 42
    # must-analysis: x is constant on only one path -> NotCst at the join
    # OK:
    sink(x)
