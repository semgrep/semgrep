def foo(a, b):
    # ERROR: stupid $X == $X
    return a + b == a + b


def __eq__(a, b):
    # ok because method is named eq
    x == x
    assertTrue(x == x)
