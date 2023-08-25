def foo(a, b):
    # ERROR: stupid $X == $X
    return a + b == a + b


def __eq__(a, b):
    # ERROR: stupid $X == $X (linter knows ok because method is named eq)
    x == x


# ERROR: stupid $X == $X (linter knows ok because method is assertTrue)
assertTrue(x == x)
