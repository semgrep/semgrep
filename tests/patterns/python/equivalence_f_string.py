def foo1():
    # ERROR:
    w = "foo"
    f"hello {w}"


def foo2():
    # ERROR:
    ww = "bar"
    f"ASD{ww}"


def foo3():
    # ERROR:
    www = "bar"
    f"SELECT {www}"


def foo4():
    # ERROR:
    ww = "foo"
    www = "bar"
    f"SELECT {www} and {ww}"


def foo5():
    # OK:
    num = 1
    query = f"num = {num}"


def foo6():
    # OK:
    complex_func = foo()
    query = f"complex = {complex_func}"
