def foo1():
    # ERROR:
    select = "select * "
    f"{select} from foo"


def foo2():
    # ERROR:
    select = "select * "
    # ERROR:
    name = "foo"
    query = f"{select} from foo where name={name}"


def foo5():
    # OK:
    num = 1
    f"{num} is 1"


def foo6():
    # OK:
    complex_func = foo()
    f"{complex_func} is foo"


def foo7():
    # ERROR:
    w = "foo"
    f"hello {w}"


def foo8():
    # ERROR:
    ww = "bar"
    f"ASD{ww}"


def foo9():
    # ERROR:
    www = "bar"
    f"SELECT {www}"


def foo10():
    # ERROR:
    ww = "foo"
    # ERROR:
    ww = "foo"
    # ERROR:
    www = "bar"
    f"SELECT {www} and {ww}"


def foo11():
    # OK:
    num = 1
    query = f"num = {num}"


def foo12():
    # OK:
    complex_func = foo()
    query = f"complex = {complex_func}"
