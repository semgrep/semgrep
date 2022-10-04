def bad():
    x = set([])
    # `x` becomes tainted by side effect
    taint(x)
    y = x
    #ruleid: test
    sink(y)

def ok():
    x = set([])
    taint(x)
    # remotes taint
    x = set([])
    #ok: test
    sink(y)
