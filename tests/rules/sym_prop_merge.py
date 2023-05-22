def test1():
    x = bar()
    if cond:
        y = 42
    #ruleid: test
    foo(x)

def test2():
    x = bar()
    while cond:
        y = 42
    #todoruleid: test
    foo(x)

def test3():
    x = bar()
    if cond1:
        x = baz()
    #ok: test
    foo(x)
