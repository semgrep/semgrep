def foo(a, b):
    # ERROR: stupid $X == $X
    return a + b == a + b
