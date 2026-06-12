# PEP 380 `yield from` delegation (Python 3.3).
def gen():
    # ERROR: match
    yield from inner()


def plain():
    yield 1
