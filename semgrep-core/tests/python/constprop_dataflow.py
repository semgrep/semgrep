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

