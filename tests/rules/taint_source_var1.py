def test():
    x = set([])
    # `x` becomes tainted by side effect, even when `taint(x)` is nested
    # inside an if conditional! This is because we no longer rely on
    # `...` which traverses a sub-AST rather than the CFG.
    if cond:
        taint(x)
    y = x
    #ruleid: test
    sink(y)
