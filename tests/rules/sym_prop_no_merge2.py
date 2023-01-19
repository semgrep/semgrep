def test1():
    if cond():
        y = f(x)
        # ruleid: test
        x = g(y)
    # OK:
    return x

def test2():
    while cond():
        y = f(x)
        # ruleid: test
        x = g(y)
    # OK:
    return x

