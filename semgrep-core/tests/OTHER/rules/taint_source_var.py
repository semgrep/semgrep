def test():
    x = set([])
    # `x` becomes tainted by side effect
    taint(x)
    y = x
    #ruleid: test
    sink(y)
