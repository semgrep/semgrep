# PEP 448 starred expression in `return` (Python 3.5).
def f():
    rest = (2, 3)
    # ERROR: match
    return 1, *rest


def g():
    return 1, 2
