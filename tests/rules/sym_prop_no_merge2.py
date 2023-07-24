def test1():
    if cond():
        y = f(x)
        # ruleid: test
        z = g(y)
    # OK:
    return z

def test2():
    while cond():
        y = f(x)
        # ruleid: test
        z = g(y)
    # OK:
    return z

