# PEP 3104 nonlocal declarations (Python 3.0).
def outer():
    x = 1

    def inner():
        # ERROR: match
        nonlocal x
        x += 1

    return inner
