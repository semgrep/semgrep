# PEP 570 positional-only parameters via the `/` separator (Python 3.8).
# ERROR: match
def basic(a, b, /, c):
    return a


def regular(a, b, c):
    return a
