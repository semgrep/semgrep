def foo1():
    # ERROR:
    x = "x"
    # ERROR:
    y = "y"
    # ERROR:
    t = x + y

def foo2(c):
    if c:
        # ERROR:
        a = "a"
    else:
        # ERROR:
        a = "b"
    # ERROR:
    v = a


def foo3(c):
    # ERROR:
    x = "hi"
    while c:
        # TODO:
        # OK:
        x = x + " hi"
    # ERROR:
    y = x
