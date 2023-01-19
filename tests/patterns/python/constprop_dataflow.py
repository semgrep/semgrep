def test1():
    x = "x"
    # ERROR:
    foo(x)
    y = "y"
    # ERROR:
    foo(y)
    t = x + y
    # ERROR:
    foo(t)

def test2(c):
    if c:
        a = "a"
        # ERROR:
        foo(a)
    else:
        a = "b"
        # ERROR:
        foo(a)
    # ERROR:
    foo(a)

def test3(c):
    if c:
        x = "hi"
    # Must analysis, and `x' is only constant in one path!
    # OK:
    foo(x)

def test4(c):
    x = "hi"
    # ERROR:
    foo(x)
    while c:
        x = x + " hi"
        # TODO: See Dataflow_svalue.input_env
        # OK:
        foo(x)
    # TODO
    # OK:
    foo(x)
