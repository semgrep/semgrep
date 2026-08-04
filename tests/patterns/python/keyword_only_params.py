# PEP 3102 keyword-only parameters via the bare `*` separator (Python 3.0).
# ERROR: match
def kwonly(a, *, b):
    return a


def regular(a, b):
    return a
